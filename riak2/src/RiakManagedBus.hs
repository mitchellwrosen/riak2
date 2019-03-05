module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , EventHandlers(..)
  , createManagedBus
  , managedBusReady
  , withBus
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..))
import RiakDebug          (debug)

import qualified RiakBus as Bus

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket_, tryAny)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import System.Mem.Weak        (Weak, deRefWeak)


data ManagedBus
  = ManagedBus
  { endpoint :: !Endpoint
  , receiveTimeout :: !Int
  , statusVar :: !(TVar Status)
  , inFlightVar :: !(TVar Int)
    -- ^ The number of in-flight requests.
  , lastUsedRef :: !(IORef Word64)
    -- ^ The last time the bus was used.
  , handlers :: !EventHandlers
  , aliveRef :: !(IORef ())
    -- ^ Used for finalization.
  }

-- Managed bus, sans canary IORef.
data ManagedBus_
  = ManagedBus_
  { endpoint :: !Endpoint
  , receiveTimeout :: !Int
  , statusVar :: !(TVar Status)
  , inFlightVar :: !(TVar Int)
  , lastUsedRef :: !(IORef Word64)
  , handlers :: !EventHandlers
  }

data Status :: Type where
  Disconnected :: Status
  Disconnecting :: Status
  Connecting :: Status
  Unhealthy :: !Bus -> Status
  Healthy :: !Bus -> Status

data ManagedBusError :: Type where
  -- | The bus is not ready to accept requests, either because it is connecting,
  -- unhealthy, or draining connections and about to reconnect.
  ManagedBusDisconnectedError :: ManagedBusError
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

-- | Create a managed bus.
--
-- /Throws/. This function will never throw an exception.
createManagedBus ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO ManagedBus
createManagedBus endpoint receiveTimeout handlers = do
  statusVar :: TVar Status <-
    newTVarIO Disconnected

  inFlightVar :: TVar Int <-
    newTVarIO 0

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  aliveRef :: IORef () <-
    newIORef ()

  pure ManagedBus
    { endpoint = endpoint
    , receiveTimeout = receiveTimeout
    , statusVar = statusVar
    , inFlightVar = inFlightVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    , aliveRef = aliveRef
    }

-- | An STM action that returns when the managed bus is connected and healthy.
managedBusReady :: ManagedBus -> STM ()
managedBusReady ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Disconnected -> retry
    Disconnecting -> retry
    Connecting -> retry
    Unhealthy _ -> retry
    Healthy _ -> pure ()

withBus ::
     forall a.
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus bus@(ManagedBus { inFlightVar, lastUsedRef, statusVar }) callback =
  readTVarIO statusVar >>= \case
    Disconnected -> do
      void (forkIO (connect bus_))
      pure (Left ManagedBusDisconnectedError)

    Disconnecting ->
      pure (Left ManagedBusDisconnectedError)

    Connecting ->
      pure (Left ManagedBusDisconnectedError)

    Unhealthy _ ->
      pure (Left ManagedBusDisconnectedError)

    Healthy bus -> do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      bracket_
        (increment inFlightVar)
        (decrement inFlightVar)
        (callback bus) >>= \case


          Left err -> do
            void (forkIO (disconnect bus_ True))
            pure (Left (fromBusError err))

          Right result ->
            pure (Right result)

  where
    bus_ :: ManagedBus_
    bus_ =
      case bus of
        ManagedBus{..} ->
          ManagedBus_{..}

    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusClosedError -> ManagedBusDisconnectedError
      BusConnectionError err -> ManagedBusConnectionError err
      BusDecodeError err -> ManagedBusDecodeError err
      BusUnexpectedResponseError -> ManagedBusUnexpectedResponseError


-- TODO give up if bus gc'd
connect ::
     ManagedBus_
  -> IO ()
connect managedBus@(ManagedBus_ { endpoint, handlers, receiveTimeout, statusVar }) =
  join . atomically $
    readTVar statusVar >>= \case
      Disconnected -> do
        writeTVar statusVar Connecting

        pure $ do
          debug "handle: connecting"
          connectLoop 1

      _ ->
        pure (pure ())

  where
    connectLoop :: NominalDiffTime -> IO ()
    connectLoop seconds =
      Bus.connect endpoint receiveTimeout busHandlers >>= \case
        Left err -> do
          void (tryAny (onConnectError handlers err))
          debug ("handle: " ++ show err ++ ", reconnecting in " ++ show seconds)
          sleep seconds
          connectLoop (seconds * 1.5)

        Right bus -> do
          atomically (writeTVar statusVar (Unhealthy bus))
          pingLoop seconds

    pingLoop :: NominalDiffTime -> IO ()
    pingLoop seconds =
      readTVarIO statusVar >>= \case
        Unhealthy bus ->
          -- tryAny because an outstanding ping isn't tracked by in-flight
          -- requests (so it doesn't prevent idle collection), so it's possible
          -- the file descriptor gets closed and this throws EBADF
          tryAny (Bus.ping bus) >>= \case
            Left _ ->
              pure ()

            Right (Left err) -> do
              debug ("handle: " ++ show err)
              disconnect managedBus False
              debug ("handle: reconnecting in " ++ show seconds)
              sleep seconds
              connectLoop (seconds * 1.5)

            Right (Right (Left err)) -> do
              debug ("handle: " ++ show err ++ ", pinging in " ++ show seconds)
              sleep seconds
              pingLoop (seconds * 1.5)

            Right (Right (Right _)) -> do
              debug "handle: healthy"
              atomically (writeTVar statusVar (Healthy bus))
              void (forkIO (monitorHealth managedBus))
              void (forkIO (idleTimeout managedBus))

        _ ->
          pure ()

    busHandlers :: Bus.EventHandlers
    busHandlers =
      Bus.EventHandlers
        { Bus.onSend = onSend handlers
        , Bus.onReceive = onReceive handlers
        , Bus.onConnectionError = onConnectionError handlers
        }

unhealthy ::
     ManagedBus_
  -> IO ()
unhealthy managedBus@(ManagedBus_ { statusVar }) = do
  debug "handle: pinging until healthy"
  loop 1

  where
    loop :: NominalDiffTime -> IO ()
    loop seconds =
      readTVarIO statusVar >>= \case
        Unhealthy bus ->
          -- tryAny because an outstanding ping isn't tracked by in-flight
          -- requests (so it doesn't prevent idle collection), so it's possible
          -- the file descriptor gets closed and this throws EBADF
          tryAny (Bus.ping bus) >>= \case
            Left _ ->
              pure ()

            Right (Left err) -> do
              debug ("handle: " ++ show err)
              disconnect managedBus True

            Right (Right (Left err)) -> do
              debug ("handle: " ++ show err ++ ", pinging in " ++ show seconds)
              sleep seconds
              loop (seconds * 1.5)

            Right (Right (Right _)) -> do
              debug "handle: healthy"
              atomically (writeTVar statusVar (Healthy bus))
              void (forkIO (monitorHealth managedBus))
              void (forkIO (idleTimeout managedBus))

        _ ->
          pure ()

monitorHealth ::
     ManagedBus_
  -> IO ()
monitorHealth managedBus@(ManagedBus_ { statusVar }) =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable ping frequency
      threadDelay (1*1000*1000)

      readTVarIO statusVar >>= \case
        Healthy bus ->
          tryAny (Bus.ping bus) >>= \case
            Left _ ->
              pure ()

            Right (Left err) -> do
              debug ("handle: health check failed: " ++ show err)
              healthyToUnhealthy
              disconnect managedBus True

            Right (Right (Left err)) -> do
              debug ("handle: health check failed: " ++ show err)
              healthyToUnhealthy
              unhealthy managedBus

            Right (Right (Right _)) ->
              loop

        _ ->
          pure ()

    healthyToUnhealthy :: IO ()
    healthyToUnhealthy =
      atomically $
        readTVar statusVar >>= \case
          Healthy bus ->
            writeTVar statusVar (Unhealthy bus)
          _ ->
            pure ()

idleTimeout ::
     ManagedBus_
  -> IO ()
idleTimeout bus@(ManagedBus_ { inFlightVar, lastUsedRef, statusVar }) =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable idle timeout
      threadDelay (5*1000*1000)

      readTVarIO statusVar >>= \case
        Unhealthy _ ->
          join . atomically $
            readTVar statusVar >>= \case
              Unhealthy _ ->
                retry
              Healthy _ ->
                pure loop
              _ ->
                pure (pure ())

        Healthy _ -> do
          now <- getMonotonicTimeNSec
          lastUsed <- readIORef lastUsedRef

          if now - lastUsed > (10*1000*1000*1000)
            then do
              join . atomically $
                readTVar inFlightVar >>= \case
                  -- We have no in-flight requests, and the idle timeout has
                  -- elapsed.
                  0 -> do
                    pure (disconnect bus False)

                  -- We have an in-flight request, so even though the idle
                  -- timeout has elapsed, just pretend it hasn't.
                  _ ->
                    pure loop
            else
              loop

        Disconnected -> pure ()
        Disconnecting -> pure ()
        Connecting -> pure ()


disconnect ::
     ManagedBus_
  -> Bool
  -> IO ()
disconnect managedBus@(ManagedBus_ { inFlightVar, statusVar }) reconnectAfter =
  join . atomically $
    readTVar statusVar >>= \case
      Unhealthy bus -> do
        writeTVar statusVar Disconnecting
        pure (doDisconnect bus)

      Healthy bus -> do
        writeTVar statusVar Disconnecting
        pure (doDisconnect bus)

      Disconnected -> pure (pure ())
      Connecting -> pure (pure ())
      Disconnecting -> pure (pure ())

  where
    doDisconnect :: Bus -> IO ()
    doDisconnect bus = do
      debug "handle: draining connections"
      waitForZero inFlightVar
      debug "handle: disconnecting"
      void (tryAny (Bus.disconnect bus))
      atomically (writeTVar statusVar Disconnected)
      when reconnectAfter (connect managedBus)

increment :: TVar Int -> IO ()
increment var =
  atomically (modifyTVar' var (+1))

decrement :: TVar Int -> IO ()
decrement var =
  atomically (modifyTVar' var (subtract 1))

waitForZero :: TVar Int -> IO ()
waitForZero var =
  atomically $
    readTVar var >>= \case
      0 -> pure ()
      _ -> retry

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
