-- |
--
-- Managed bus state machine:
--
-- [DISCONNECTED]
--   Request comes in ==> [CONNECTING]
--
-- [CONNECTING]
--   Connect failure ==> [CONNECTING]
--   Connect success ==> [UNHEALTHY]
--
-- [UNHEALTHY]
--   Ping failure ==> [DRAINING]
--   Ping nack ==> [UNHEALTHY]
--   Ping ack ==> [HEALTHY]
--
-- [HEALTHY]
--   Request failure ==> [DRAINING]
--   Ping nack ==> [UNHEALTHY]
--   Idle timeout
--     No in-flight requests ==> [DRAINING]
--     In-flight request(s) ==> [HEALTHY]
--
-- [DRAINING]
--   No in-flight requests ==> [CONNECTING]

module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , EventHandlers(..)
  , managedBusReady
  , withManagedBus
  , exchange
  , stream
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..))
import RiakDebug          (debug)

import qualified RiakBus as Bus

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception        (asyncExceptionFromException,
                                 asyncExceptionToException)
import Control.Exception.Safe   (Exception(..), SomeException, bracket, bracket_, tryAny)
import Control.Foldl            (FoldM)
import Data.Fixed               (Fixed(..))
import Data.Time.Clock          (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits             (KnownNat)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TVar Status)
  , inFlightVar :: !(TVar Int)
    -- ^ The number of in-flight requests.
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
  Unhealthy :: Status
  -- | Connected and healthy.
  Healthy :: !Bus -> Status

data ManagedBusError :: Type where
  -- | The bus is not ready to accept requests, either because it is connecting,
  -- unhealthy, or draining connections and about to reconnect.
  ManagedBusNotReadyError :: ManagedBusError
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

data EventHandlers
  = EventHandlers
  { onSend :: !(forall code. Request code -> IO ())
    -- ^ Called just prior to sending a request.
  , onReceive :: !(forall code. Response code -> IO ())
    -- ^ Called just after receiving a response.
  , onConnectError :: !(ConnectError -> IO ())
  , onConnectionError :: !(ConnectionError -> IO ())
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 <> EventHandlers a2 b2 c2 d2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)


-- | An STM action that returns when the managed bus is connected and healthy.
managedBusReady :: ManagedBus -> STM ()
managedBusReady ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Disconnected -> retry
    Connecting -> retry
    Unhealthy -> retry
    Healthy _ -> pure ()

-- | Acquire a managed bus.
--
-- /Throws/. This function will never throw an exception.
withManagedBus ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus endpoint receiveTimeout handlers callback = do
  statusVar :: TVar Status <-
    newTVarIO Disconnected

  inFlightVar :: TVar Int <-
    newTVarIO 0

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  threadId :: ThreadId <-
    myThreadId

  let
    bus :: ManagedBus
    bus =
      ManagedBus
        { statusVar = statusVar
        , inFlightVar = inFlightVar
        , lastUsedRef = lastUsedRef
        , handlers = handlers
        }

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (managerThread endpoint receiveTimeout bus)) >>= \case
          Left err ->
            throwTo threadId (ManagedBusCrashed err)
          Right void ->
            absurd void

  bracket
    acquire
    killThread
    (\_ -> callback bus)

managerThread ::
     Endpoint
  -> Int
  -> ManagedBus
  -> IO Void
managerThread
    endpoint
    receiveTimeout
    ManagedBus { handlers, inFlightVar, lastUsedRef, statusVar } =

  disconnected

  where
    connecting :: NominalDiffTime -> IO Void
    connecting seconds = do
      debug "managed bus: connecting;"

      result :: Either ConnectError (IO Void) <-
        Bus.withBus endpoint receiveTimeout busHandlers $ \bus -> do
          atomically (writeTVar statusVar Unhealthy)
          unhealthy bus 1

      case result of
        Left err -> do
          onConnectError handlers err

          debug ("managed bus: connecting; connect failed: " ++ show err)
          sleep seconds
          connecting (seconds * 1.5)

        Right next ->
          next

    healthy :: Bus -> IO (IO Void)
    healthy bus =
      join .
        withAsync (healthCheckThread statusVar bus) $ \_ ->
        withAsync (idleTimeoutThread statusVar inFlightVar lastUsedRef) $ \_ ->
          atomically $
            readTVar statusVar >>= \case
              Disconnected -> pure (pure disconnected)
              Connecting -> pure draining
              Unhealthy -> pure (unhealthy bus 1)
              Healthy _ -> retry

    unhealthy :: Bus -> NominalDiffTime -> IO (IO Void)
    unhealthy bus seconds = do
      debug "managed bus: unhealthy; pinging"

      Bus.ping bus >>= \case
        Left err -> do
          debug ("managed bus: unhealthy; ping failed: " ++ show err)
          sleep seconds
          draining

        Right (Left err) -> do
          debug ("managed bus: unhealthy; ping failed: " ++ show err)
          sleep seconds
          unhealthy bus (seconds * 1.5)

        Right (Right _) -> do
          atomically (writeTVar statusVar (Healthy bus))
          healthy bus

    draining :: IO (IO Void)
    draining = do
      debug "managed bus: draining;"

      atomically $
        readTVar inFlightVar >>= \case
          0 -> pure ()
          _ -> retry

      pure (connecting 1)

    disconnected :: IO Void
    disconnected = do
      debug "managed bus: disconnected;"

      atomically $ do
        readTVar statusVar >>= \case
          Disconnected -> retry
          Connecting -> pure ()
          Unhealthy -> undefined
          Healthy _ -> undefined

      connecting 1

    busHandlers :: Bus.EventHandlers
    busHandlers =
      Bus.EventHandlers
        { Bus.onSend = onSend handlers
        , Bus.onReceive = onReceive handlers
        , Bus.onConnectionError = onConnectionError handlers
        }

healthCheckThread ::
     TVar Status
  -> Bus
  -> IO ()
healthCheckThread statusVar bus = do
  debug "managed bus: healthy; spawned health check thread"
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
              Unhealthy -> undefined
              Healthy _ -> writeTVar statusVar Connecting

        Right (Left err) -> do
          debug ("managed bus: healthy; ping failed: " ++ show err)

          atomically $ do
            readTVar statusVar >>= \case
              Disconnected -> pure ()
              Connecting -> pure ()
              Unhealthy -> undefined
              Healthy _ -> writeTVar statusVar Unhealthy

        Right (Right _) ->
          loop

idleTimeoutThread ::
     TVar Status
  -> TVar Int
  -> IORef Word64
  -> IO ()
idleTimeoutThread statusVar inFlightVar lastUsedRef = do
  debug "managed bus: healthy; spawning idle timeout thread"
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
          next :: IO () <-
            atomically $
              readTVar inFlightVar >>= \case
                -- We have no in-flight requests, and the idle timeout has
                -- elapsed. Set the status to Disconnected, but only if it's
                -- still Healthy.
                0 -> do
                  readTVar statusVar >>= \case
                    Disconnected -> pure ()
                    Connecting -> pure ()
                    Unhealthy -> pure ()
                    Healthy _ -> writeTVar statusVar Disconnected

                  pure (pure ())

                -- We have an in-flight request, so even though the idle timeout
                -- has elapsed, just pretend it hasn't.
                _ ->
                  pure loop

          next
        else
          loop

withBus ::
     forall a.
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus ManagedBus { inFlightVar, lastUsedRef, statusVar } callback =
  join . atomically $
    readTVar statusVar >>= \case
      Disconnected -> do
        writeTVar statusVar Connecting
        pure (pure (Left ManagedBusNotReadyError))

      Connecting ->
        pure (pure (Left ManagedBusNotReadyError))

      Unhealthy ->
        pure (pure (Left ManagedBusNotReadyError))

      Healthy bus -> pure $ do
        writeIORef lastUsedRef =<< getMonotonicTimeNSec

        -- It's ok that this code is not exception-safe. If a callback results
        -- in a bus error, and then we are killed before writing 'Connecting' to
        -- the status var, the next thread that comes along will attempt to use
        -- the dead connection and see a similar bus error.
        doCallback bus >>= \case
          Left err -> do
            atomically $
              modifyTVar' statusVar $ \case
                Disconnected -> Disconnected
                Connecting -> Connecting
                Unhealthy -> Unhealthy
                Healthy _ -> Connecting
            pure (Left (fromBusError err))
          Right result ->
            pure (Right result)

  where
    doCallback :: Bus -> IO (Either BusError a)
    doCallback bus =
      bracket_
        (atomically (modifyTVar' inFlightVar (+1)))
        (atomically (modifyTVar' inFlightVar (subtract 1)))
        (callback bus)

    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusClosedError -> ManagedBusNotReadyError
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
