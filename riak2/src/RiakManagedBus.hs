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
import Data.Fixed               (Fixed(..))
import Data.Time.Clock          (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits             (KnownNat)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TVar Status)
  , lastUsedRef :: !(IORef Word64)
    -- ^ The last time the bus was used.
  , handlers :: !EventHandlers
  }

data Status :: Type where
  -- | Disconnected until a request comes in.
  Disconnected :: Status
  -- | Attempting to establish a connection.
  Connecting :: Status
  -- | Connected.
  Connected :: !Bus -> Status

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
    Disconnected -> retry
    Connecting -> retry
    Connected{} -> pure ()

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
    newTVarIO Disconnected

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  threadId :: ThreadId <-
    myThreadId

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (managerThread endpoint handlers statusVar lastUsedRef)) >>= \case
          Left err ->
            throwTo threadId (ManagedBusCrashed err)
          Right void ->
            absurd void

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
        , lastUsedRef = lastUsedRef
        , handlers = handlers
        })

managerThread ::
     Endpoint
  -> EventHandlers
  -> TVar Status
  -> IORef Word64
  -> IO Void
managerThread endpoint handlers statusVar lastUsedRef =
  idle

  where
    -- Reconnecting state:
    --
    -- * Given a length of time to sleep for if connecting fails, attempt to
    --   establish a connection.
    --
    -- * If a connection is established, ping Riak. If the ping succeeds, move
    --   to the connected state. If the ping fails, sleep and retry (with a
    --   longer sleep time).
    --
    -- * If the connection is not established, sleep and retry (with a longer
    --   sleep time).
    reconnecting :: NominalDiffTime -> IO Void
    reconnecting seconds = do
      debug "managed bus: connecting"

      result :: Either ConnectError (Maybe (IO Void)) <-
        Bus.withBus endpoint handlers $ \bus -> do
          debug "managed bus: connected, making initial ping"

          Bus.ping bus >>= \case
            Left err -> do
              debug ("managed bus: initial ping failed: " ++ show err)
              pure Nothing

            Right (Left err) -> do
              debug ("managed bus: initial ping failed: " ++ show err)
              pure Nothing

            Right (Right _) -> do
              debug "managed bus: initial ping succeeded"
              Just <$> connected bus

      case result of
        Left err -> do
          debug ("managed bus: connect failed: " ++ show err)
          sleep seconds
          reconnecting (seconds * 1.5)

        Right Nothing -> do
          sleep seconds
          reconnecting (seconds * 1.5)

        Right (Just next) ->
          next

    -- Connected state:
    --
    -- * Spawn a health-check thread.
    --
    -- * Spawn an idle timeout thread.
    --
    -- * Record that the connection is established, so clients can begin using
    --   it.
    --
    -- * Wait for the connection status to update to either 'Disconnected' (idle
    --   timeout) or 'Connecting' (connection failure). Return the action to
    --   be called next.
    connected :: Bus -> IO (IO Void)
    connected bus = do
      debug "managed bus: spawning health check thread"

      withAsync (healthCheckThread statusVar bus) $ \_ -> do
        debug "managed bus: spawning idle timeout thread"

        withAsync (idleTimeoutThread statusVar lastUsedRef) $ \_ -> do
          atomically (writeTVar statusVar (Connected bus))

          atomically $ do
            readTVar statusVar >>= \case
              Disconnected -> pure idle
              Connecting -> pure (reconnecting 1)
              Connected{} -> retry

    -- Idle state:
    --
    -- * Wait for status to go from 'Disconnected' to 'Connecting'.
    --
    -- * Move to reconnecting state.
    idle :: IO Void
    idle = do
      debug "managed bus: idle"

      atomically $ do
        readTVar statusVar >>= \case
          Disconnected -> retry
          Connecting -> pure ()
          Connected{} -> undefined

      reconnecting 1

healthCheckThread ::
     TVar Status
  -> Bus
  -> IO ()
healthCheckThread statusVar bus =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable ping frequency
      threadDelay (3*1000*1000)

      debug "managed bus: health check thread: pinging"
      Bus.ping bus >>= \case
        Left err -> do
          debug ("managed bus: health check thread: ping failed: " ++ show err)

          atomically $
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Connected{} -> writeTVar statusVar Connecting

        -- TODO manager thread new state: "unhealthy"
        Right response -> do
          debug ("managed bus: health check thread: ping succeeded: " ++ show response)
          loop

idleTimeoutThread ::
     TVar Status
  -> IORef Word64
  -> IO ()
idleTimeoutThread statusVar lastUsedRef =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable idle timeout
      threadDelay (5*1000*1000)

      debug "managed bus: idle timeout thread: checking last used time"

      now <- getMonotonicTimeNSec
      lastUsed <- readIORef lastUsedRef

      if now - lastUsed > (10*1000*1000*1000)
        then do
          debug "managed bus: idle timeout thread: closing idle connection"

          atomically $
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Connected _ -> writeTVar statusVar Disconnected
        else
          loop

withBus ::
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus ManagedBus { lastUsedRef, statusVar } callback =
  readTVarIO statusVar >>= \case
    Disconnected -> do
      atomically (writeTVar statusVar Connecting)
      pure (Left ManagedBusConnectingError)

    Connecting ->
      pure (Left ManagedBusConnectingError)

    Connected bus -> do
      writeIORef lastUsedRef =<< getMonotonicTimeNSec

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
      BusClosedError -> ManagedBusConnectingError
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
