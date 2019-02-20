-- | A Riak client that manages another client by reconnecting automatically.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Handle.Impl.Managed
  ( Handle
  , HandleConfig(..)
  , ReconnectSettings(..)
  , HandleConnectError
  , HandleConnectionError
  , withHandle
  , exchange
  , stream
  , ManagedHandleCrashed(..)
  ) where

import Libriak.Request  (Request)
import Libriak.Response (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket, tryAny)
import Control.Monad          (when)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, UTCTime, diffUTCTime,
                               getCurrentTime, nominalDiffTimeToSeconds)
import Numeric.Natural        (Natural)


data Handle
  = Handle
  { statusVar :: !(TMVar HandleStatus)
  , errorVar :: !(TMVar Handle.HandleConnectionError)
    -- ^ The last error some client received when trying to use the handle.
  }

-- Either the handle and which "generation" it is (when 0 dies, 1 replaces it,
-- etc), or the connection that brought down the manager thread.
data HandleStatus
  = HandleDead Handle.HandleConnectError
  | HandleAlive Handle.Handle Natural

data HandleConfig
  = HandleConfig
  { innerConfig :: !Handle.HandleConfig
    -- ^ The inner handle's config.
  , reconnectSettings ::
      !(Handle.HandleConnectError -> Maybe ReconnectSettings)
    -- ^ How to behave when an error occurs. 'Nothing' means don't reconnect.
  }

data ReconnectSettings
  = ReconnectSettings
  { initialDelay :: !NominalDiffTime
    -- ^ How long to delay before reconnecting for the first time.
  , retryFor :: !NominalDiffTime
    -- ^ Attempt to reconnect periodically until this amount of time has passed.
  }

type HandleConnectError
  = Handle.HandleConnectError

type HandleConnectionError
  = Handle.HandleConnectError

newtype ManagedHandleCrashed
  = ManagedHandleCrashed SomeException
  deriving stock (Show)

instance Exception ManagedHandleCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
--
-- /Throws/. If the background manager thread crashes, which indicates there is
-- a bug in the library, throws an asynchronous 'ManagedHandleCrashed'
-- exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either Handle.HandleConnectError a)
withHandle config onSuccess = do
  statusVar :: TMVar HandleStatus <-
    newEmptyTMVarIO

  errorVar :: TMVar Handle.HandleConnectionError <-
    newEmptyTMVarIO

  threadId :: ThreadId <-
    myThreadId

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (manager config statusVar errorVar)) >>= \case
          Left err ->
            throwTo threadId (ManagedHandleCrashed err)
          Right () ->
            pure ()

  let
    release :: ThreadId -> IO ()
    release =
      killThread

  bracket
    acquire
    release
    (\_ -> do
      Right <$>
        onSuccess Handle
          { statusVar = statusVar
          , errorVar = errorVar
          })

-- The manager thread:
--
-- * Acquire an underlying connection.
-- * Smuggle it out to the rest of the world via a TMVar.
-- * Wait for an error to appear in another TMVar, then reconnect.
--
-- Meanwhile, users of this handle (via exchange/stream) grab the underlying
-- handle (if available), use it, and if anything goes wrong, write to the error
-- TMVar and retry when a new connection is established.
--
-- TODO notify waiting clients of the shutdown
manager ::
     HandleConfig
  -> TMVar HandleStatus
  -> TMVar Handle.HandleConnectionError
  -> IO ()
manager (HandleConfig innerConfig reconnectSettings) statusVar errorVar = do
  err <- loop 0 Nothing
  atomically (putTMVar statusVar (HandleDead err))

  where
    loop ::
         Natural
         -- ^ The handle generation. Every time we successfully reconnect, it's
         -- incremented by 1.
      -> Maybe (UTCTime, NominalDiffTime, Handle.HandleConnectError)
         -- ^ The last time we tried to reconnect, how long we should sleep for
         -- if we decide to try again, and the original cause.
      -> IO Handle.HandleConnectError
    loop !generation prev =
      Handle.withHandle innerConfig (runUntilError generation) >>= \case
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

    runUntilError :: Natural -> Handle.Handle -> IO Handle.HandleConnectionError
    runUntilError generation handle = do
      -- Clear out any previous errors, and put the healthy handle for
      -- clients to use
      atomically $ do
        _ <- tryTakeTMVar errorVar
        putTMVar statusVar (HandleAlive handle generation)

      -- When a client records an error, remove the handle so no more
      -- clients use it (don't bother clearing out the error var yet)
      atomically $ do
        err <- readTMVar errorVar
        _ <- takeTMVar statusVar
        pure err



-- | Send a request and receive the response (a single message).
exchange ::
     Handle
  -> Request
  -> IO (Either Handle.HandleConnectError Response)
exchange Handle { statusVar, errorVar } request =
  loop 0

  where
    loop :: Natural -> IO (Either Handle.HandleConnectError Response)
    loop !healthyGen =
      waitForGen healthyGen statusVar >>= \case
        Left err ->
          pure (Left err)

        Right (handle, gen) ->
          Handle.exchange handle request >>= \case
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
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either Handle.HandleConnectError r)
stream Handle { statusVar, errorVar } request value step =
  loop 0

  where
    loop :: Natural -> IO (Either Handle.HandleConnectError r)
    loop !healthyGen =
      waitForGen healthyGen statusVar >>= \case
        Left err ->
          pure (Left err)

        Right (handle, gen) ->
          Handle.stream handle request value step >>= \case
            Left err -> do
              -- Notify the manager thread of an error (try put, because it's ok
              -- if we are not the first thread to do so)
              _ <- atomically (tryPutTMVar errorVar err)

              -- Try again once the connection is re-established.
              loop (gen + 1)

            Right response ->
              pure (Right response)

-- Wait for (at least) the given generation of handle.
waitForGen ::
     Natural
  -> TMVar HandleStatus
  -> IO (Either Handle.HandleConnectError (Handle.Handle, Natural))
waitForGen healthyGen statusVar =
  atomically $
    readTMVar statusVar >>= \case
      HandleDead err ->
        pure (Left err)

      HandleAlive handle gen -> do
        when (gen < healthyGen) retry
        pure (Right (handle, gen))

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
