module RiakManagedBus
  ( ManagedBus
  , ReconnectSettings(..)
  , withManagedBus
  , exchange
  , stream
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, Endpoint)
import RiakBus            (Bus, BusError, withBus)
import RiakRequest        (Request)
import RiakResponse       (Response)

import qualified RiakBus       as Bus

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket, tryAny)
import Control.Foldl          (FoldM)
import Control.Monad          (when)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, UTCTime, diffUTCTime,
                               getCurrentTime, nominalDiffTimeToSeconds)
import GHC.TypeLits           (KnownNat)
import Numeric.Natural        (Natural)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TMVar Status)
  , errorVar :: !(TMVar BusError)
    -- ^ The last error some client received when trying to use the bus.
  }

-- Either the bus and which "generation" it is (when 0 dies, 1 replaces it,
-- etc), or the connection that brought down the manager thread.
data Status
  = Dead ConnectError
  | Alive Bus Natural

data ReconnectSettings
  = ReconnectSettings
  { initialDelay :: !NominalDiffTime
    -- ^ How long to delay before reconnecting for the first time.
  , retryFor :: !NominalDiffTime
    -- ^ Attempt to reconnect periodically until this amount of time has passed.
  }

-- | The bus manager thread crashed, which indicates a bug in this library.
newtype ManagedBusCrashed
  = ManagedBusCrashed SomeException
  deriving stock (Show)

instance Exception ManagedBusCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | Acquire a managed bus.
--
-- /Throws/. This function will never throw an exception.
withManagedBus ::
     Endpoint
  -> (ConnectError -> Maybe ReconnectSettings)
  -- ^ How to behave when an error occurs. 'Nothing' means don't reconnect.
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus endpoint reconnectSettings onSuccess = do
  statusVar :: TMVar Status <-
    newEmptyTMVarIO

  errorVar :: TMVar BusError <-
    newEmptyTMVarIO

  threadId :: ThreadId <-
    myThreadId

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (manager endpoint reconnectSettings statusVar errorVar)) >>= \case
          Left err ->
            throwTo threadId (ManagedBusCrashed err)
          Right () ->
            pure ()

  let
    release :: ThreadId -> IO ()
    release =
      killThread

  bracket
    acquire
    release
    (\_ ->
      onSuccess ManagedBus
        { statusVar = statusVar
        , errorVar = errorVar
        })

-- The manager thread:
--
-- * Acquire an underlying connection.
-- * Smuggle it out to the rest of the world via a TMVar.
-- * Wait for an error to appear in another TMVar, then reconnect.
--
-- Meanwhile, users of this bus (via exchange/stream) grab the underlying bus
-- (if available), use it, and if anything goes wrong, write to the error TMVar
-- and retry when a new connection is established.
manager ::
     Endpoint
  -> (ConnectError -> Maybe ReconnectSettings)
  -> TMVar Status
  -> TMVar BusError
  -> IO ()
manager endpoint reconnectSettings statusVar errorVar = do
  err <- loop 0 Nothing
  atomically (putTMVar statusVar (Dead err))

  where
    loop ::
         Natural
         -- ^ The bus generation. Every time we successfully reconnect, it's
         -- incremented by 1.
      -> Maybe (UTCTime, NominalDiffTime, ConnectError)
         -- ^ The last time we tried to reconnect, how long we should sleep for
         -- if we decide to try again, and the original cause.
      -> IO ConnectError
    loop !generation prev =
      withBus endpoint (runUntilError generation) >>= \case
        Left connectErr -> do
          putStrLn ("[debug] " ++ show connectErr)

          case reconnectSettings connectErr of
            Nothing ->
              pure connectErr

            Just (ReconnectSettings initialDelay retryFor) ->
              case prev of
                -- This is not the first failure, in fact, it is a repeat.
                -- Consult the retry settings for whether we should try again
                -- or give up.
                Just (t0, sleepFor, prevErr) | connectErr == prevErr -> do
                  t1 <- getCurrentTime
                  if t1 `diffUTCTime` t0 <= retryFor
                    then do
                      sleep sleepFor
                      loop generation (Just (t0, sleepFor * 1.5, prevErr))
                    else
                      pure connectErr

                -- This is either the first failure, or a different failure than
                -- last time. Sleep for the initial delay, then re-enter the
                -- loop with an initial sleep time of 1 second.
                _ -> do
                  sleep initialDelay
                  t0 <- getCurrentTime
                  loop generation (Just (t0, initialDelay * 1.5, connectErr))

        Right connectionErr -> do
          putStrLn ("[debug] " ++ show connectionErr)
          sleep 1 -- Whoops, kind of want to exponentially back of here too
          loop (generation+1) Nothing

    runUntilError :: Natural -> Bus -> IO BusError
    runUntilError generation bus = do
      -- Clear out any previous errors, and put the healthy bus for clients to
      -- use
      atomically $ do
        _ <- tryTakeTMVar errorVar
        putTMVar statusVar (Alive bus generation)

      -- When a client records an error, remove the bus so no more clients use
      -- it (don't bother clearing out the error var yet)
      atomically $ do
        err <- readTMVar errorVar
        _ <- takeTMVar statusVar
        pure err

-- | Send a request and receive the response (a single message).
exchange ::
     forall code.
     KnownNat code
  => ManagedBus
  -> Request code
  -> IO (Either ConnectError (Either ByteString (Response code)))
exchange ManagedBus { statusVar, errorVar } request =
  loop 0

  where
    loop ::
         Natural
      -> IO (Either ConnectError (Either ByteString (Response code)))
    loop !healthyGen =
      waitForGen healthyGen statusVar >>= \case
        Left err ->
          pure (Left err)

        Right (bus, gen) ->
          Bus.exchange bus request >>= \case
            Left err -> do
              -- Notify the manager thread of an error (try put, because it's ok
              -- if we are not the first thread to do so)
              _ <- atomically (tryPutTMVar errorVar err)

              -- Try again once the connection is re-established.
              loop (gen + 1)

            Right response ->
              pure (Right response)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => ManagedBus -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either ConnectError (Either ByteString r))
stream ManagedBus { statusVar, errorVar } request responseFold =
  loop 0

  where
    loop :: Natural -> IO (Either ConnectError (Either ByteString r))
    loop !healthyGen =
      waitForGen healthyGen statusVar >>= \case
        Left err ->
          pure (Left err)

        Right (bus, gen) ->
          Bus.stream bus request responseFold >>= \case
            Left err -> do
              -- Notify the manager thread of an error (try put, because it's ok
              -- if we are not the first thread to do so)
              _ <- atomically (tryPutTMVar errorVar err)

              -- Try again once the connection is re-established.
              loop (gen + 1)

            Right response ->
              pure (Right response)

-- Wait for (at least) the given generation of bus.
waitForGen ::
     Natural
  -> TMVar Status
  -> IO (Either ConnectError (Bus, Natural))
waitForGen healthyGen statusVar =
  atomically $
    readTMVar statusVar >>= \case
      Dead err ->
        pure (Left err)

      Alive bus gen -> do
        when (gen < healthyGen) retry
        pure (Right (bus, gen))

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
