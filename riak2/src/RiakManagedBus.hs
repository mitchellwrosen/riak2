module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , managedBusReady
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
  -- | Connected, but unhealthy (haven't gotten a successful response from the
  -- initial ping).
  Unhealthy :: !Bus -> Status
  -- | Connected and healthy.
  Healthy :: !Bus -> Status

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
  -- | The bus is connected, but unhealthy.
  ManagedBusUnhealthyError :: ManagedBusError
  deriving stock (Eq, Show)

-- | The bus manager thread crashed, which indicates a bug in this library.
newtype ManagedBusCrashed
  = ManagedBusCrashed SomeException
  deriving stock (Show)

instance Exception ManagedBusCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | An STM action that returns when the managed bus is connected and healthy.
managedBusReady :: ManagedBus -> STM ()
managedBusReady ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Disconnected -> retry
    Connecting -> retry
    Unhealthy _ -> retry
    Healthy _ -> pure ()

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
    -- * Attempt to establish a connection.
    --
    -- * If successful, move to the unhealthy state (we assume even pinging will
    --   fail until proven otherwise).
    --
    -- * If unsuccessful, sleep and try again.
    reconnecting :: NominalDiffTime -> IO Void
    reconnecting seconds = do
      debug "managed bus: reconnecting;"

      result :: Either ConnectError (IO Void) <-
        Bus.withBus endpoint handlers $ \bus -> do
          atomically (writeTVar statusVar (Unhealthy bus))
          unhealthy bus 1

      case result of
        Left err -> do
          debug ("managed bus: reconnecting; connect failed: " ++ show err)
          sleep seconds
          reconnecting (seconds * 1.5)

        Right next ->
          next

    -- Healthy state:
    --
    -- * Spawn a health-check thread and an idle timeout thread.
    --
    -- * Record that the connection is established, so clients can begin using
    --   it.
    --
    -- * More docs go here.
    healthy :: Bus -> IO (IO Void)
    healthy bus = do
      debug "managed bus: healthy; spawning health check thread"

      next :: IO (IO Void) <-
        withAsync (healthCheckThread statusVar bus) $ \_ -> do
          debug "managed bus: healthy; spawning idle timeout thread"

          withAsync (idleTimeoutThread statusVar lastUsedRef) $ \_ -> do
            atomically (writeTVar statusVar (Healthy bus))

            atomically $
              readTVar statusVar >>= \case
                Disconnected -> pure (pure idle)
                Connecting -> pure (pure (reconnecting 1))
                Unhealthy _ -> pure (unhealthy bus 1)
                Healthy _ -> retry

      next

    unhealthy :: Bus -> NominalDiffTime -> IO (IO Void)
    unhealthy bus seconds = do
      debug "managed bus: unhealthy; pinging"

      Bus.ping bus >>= \case
        Left err -> do
          debug ("managed bus: unhealthy; ping failed: " ++ show err)
          sleep seconds
          pure (reconnecting 1)

        Right (Left err) -> do
          debug ("managed bus: unhealthy; ping failed: " ++ show err)
          sleep seconds
          unhealthy bus (seconds * 1.5)

        Right (Right _) ->
          healthy bus

    -- Idle state:
    --
    -- * Wait for status to go from disconnected to connecting
    --
    -- * Move to reconnecting state.
    idle :: IO Void
    idle = do
      debug "managed bus: idle;"

      atomically $ do
        readTVar statusVar >>= \case
          Disconnected -> retry
          Connecting -> pure ()
          Unhealthy _ -> undefined
          Healthy _ -> undefined

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

      debug "managed bus: healthy; pinging"

      Bus.ping bus >>= \case
        Left err -> do
          debug ("managed bus: healthy; ping failed: " ++ show err)

          atomically $
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Unhealthy _ -> undefined
              Healthy _ -> writeTVar statusVar Connecting

        Right (Left err) -> do
          debug ("managed bus: healthy; ping failed: " ++ show err)

          atomically $ do
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Unhealthy _ -> undefined
              Healthy bus -> writeTVar statusVar (Unhealthy bus)

        Right (Right _) ->
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

      now <- getMonotonicTimeNSec
      lastUsed <- readIORef lastUsedRef

      if now - lastUsed > (10*1000*1000*1000)
        then do
          debug "managed bus: healthy; closing idle connection"

          atomically $
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Unhealthy _ -> pure ()
              Healthy _ -> writeTVar statusVar Disconnected
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

    Unhealthy _ ->
      pure (Left ManagedBusUnhealthyError)

    Healthy bus -> do
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
