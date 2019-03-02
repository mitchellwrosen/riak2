module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , managedBusConnected
  , withManagedBus
  , exchange
  , stream
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..), EventHandlers(..))
import RiakDebug          (debug)

import qualified RiakBus as Bus

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception        (asyncExceptionFromException,
                                 asyncExceptionToException)
import Control.Exception.Safe   (Exception(..), SomeException, bracket, tryAny)
import Control.Foldl            (FoldM)
import Control.Monad            (forever)
import Data.Fixed               (Fixed(..))
import Data.Time.Clock          (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits             (KnownNat)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TVar Status)
  , handlers :: !EventHandlers
  }

data Status :: Type where
  Connecting :: Status
  Alive :: !Bus -> Status

data ManagedBusError :: Type where
  -- | The bus is currently connecting.
  ManagedBusConnectingError :: ManagedBusError
  -- | A connection error occurred during a send or receive.
  ManagedBusConnectionError :: !ConnectionError -> ManagedBusError
  -- | A protobuf decode error occurred.
  ManagedBusDecodeError :: !DecodeError -> ManagedBusError
  -- | A response with an unexpcected message code was received.
  -- TODO put request/response inside
  ManagedBusUnexpectedResponseError :: ManagedBusError
  deriving stock (Eq, Show)

-- | The bus manager thread crashed, which indicates a bug in this library.
newtype ManagedBusCrashed
  = ManagedBusCrashed SomeException
  deriving stock (Show)

instance Exception ManagedBusCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | An STM action that returns when the managed bus is connected.
managedBusConnected :: ManagedBus -> STM ()
managedBusConnected ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Connecting -> retry
    Alive{} -> pure ()

-- | Acquire a managed bus.
--
-- /Throws/. This function will never throw an exception.
withManagedBus ::
     Endpoint
  -> EventHandlers
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus endpoint handlers callback = do
  statusVar :: TVar Status <-
    newTVarIO Connecting

  threadId :: ThreadId <-
    myThreadId

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (manager endpoint handlers statusVar)) >>= \case
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
      callback ManagedBus
        { statusVar = statusVar
        , handlers = handlers
        })

manager ::
     Endpoint
  -> EventHandlers
  -> TVar Status
  -> IO a
manager endpoint handlers statusVar =
  forever (reconnecting 1 connected)

  where
    -- Reconnecting state
    reconnecting ::
         forall r.
         NominalDiffTime
      -> (Bus -> IO r)
      -> IO r
    reconnecting seconds callback = do
      result :: Either ConnectError (Maybe r) <-
        Bus.withBus endpoint handlers $ \bus ->
          Bus.ping bus >>= \case
            Left err -> do
              debug (show err ++ ", sleeping for " ++ show seconds)
              pure Nothing

            Right (Left err) -> do
              debug (show err ++ ", sleeping for " ++ show seconds)
              pure Nothing

            Right (Right _) ->
              Just <$> callback bus

      case result of
        Left err -> do
          debug (show err ++ ", sleeping for " ++ show seconds)
          sleep seconds
          reconnecting (seconds * 1.5) callback

        Right Nothing -> do
          sleep seconds
          reconnecting (seconds * 1.5) callback

        Right (Just result) ->
          pure result

    -- Connected state:
    --
    -- * Spawn a health-check thread.
    --
    -- * Wait for the status var to be set to 'Connecting' (by some thread,
    --   possibly the health-check thread, whose request failed).
    connected :: Bus -> IO ()
    connected bus =
      withAsync (healthCheckThread bus) $ \_ -> do
        atomically (writeTVar statusVar (Alive bus))

        atomically $ do
          readTVar statusVar >>= \case
            Connecting -> pure ()
            Alive{} -> retry

    healthCheckThread :: Bus -> IO ()
    healthCheckThread bus =
      loop

      where
        loop :: IO ()
        loop = do
          -- TODO configurable ping frequency
          threadDelay (3*1000*1000)

          Bus.ping bus >>= \case
            Left err -> do
              debug ("ping " ++ show err)
              atomically (writeTVar statusVar Connecting)
              pure ()

            -- TODO manager thread third state: "unhealthy"
            Right response -> do
              debug ("ping " ++ show response)
              loop

withBus ::
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus ManagedBus { statusVar } callback =
  readTVarIO statusVar >>= \case
    Connecting ->
      pure (Left ManagedBusConnectingError)

    Alive bus ->
      -- It's ok that this code is not exception-safe. If a callback results in
      -- a bus error, and then we are killed before writing 'Connecting' to the
      -- status var, the next thread that comes along will attempt to use the
      -- dead connection and see a similar bus error.
      callback bus >>= \case
        Left err -> do
          atomically (writeTVar statusVar Connecting)
          pure (Left (fromBusError err))
        Right result ->
          pure (Right result)

  where
    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusConnectionError err -> ManagedBusConnectionError err
      BusDecodeError err -> ManagedBusDecodeError err
      BusUnexpectedResponseError -> ManagedBusUnexpectedResponseError


-- | Send a request and receive the response (a single message).
exchange ::
     forall code.
     KnownNat code
  => ManagedBus
  -> Request code
  -> IO (Either ManagedBusError (Either (Response 0) (Response code)))
exchange managedBus request =
  withBus managedBus (\bus -> Bus.exchange bus request)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => ManagedBus -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either ManagedBusError (Either (Response 0) r))
stream managedBus request responseFold =
  withBus managedBus (\bus -> Bus.stream bus request responseFold)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
