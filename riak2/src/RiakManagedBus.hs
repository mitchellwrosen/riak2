-- |
--
-- The managed bus state machine.
--
-- [Disconnected]
--   We are not connected, because no requests have been made yet, or we idly
--   timed out.
--
--   When a request is received,
--     ==> [Connecting]
--
-- [Connecting]
--   We are attempting to connect to Riak.
--
--   If the connect succeeds,
--     ==> [Foreground Health Checking]
--
--   If the connect fails,
--     ==> [Connecting]
--
--   TODO eventually stop trying to connect
--
-- [Foreground Health Checking]
--   We are either attempting to successfully ping the server for the first
--   time before declaring ourselves healthy, or the background health check
--   failed.
--
--   If a ping fails with a connection error,
--     ==> [Reconnecting]
--
--   If a ping fails with a Riak error,
--     ==> [Foreground Health Checking]
--
--   If a ping succeeds,
--     ==> [Connected]
--
-- [Connected]
--   We are connected and healthy. This is the only state that we successfully
--   accept requests in. We fork two background threads for periodic health
--   checking and idle timeout.
--
--   If a request fails with a connection error,
--     ==> [Reconnecting]
--
--   If a ping fails with a connection error,
--     ==> [Reconnecting]
--
--   If a ping fails with a Riak error,
--     ==> [Foreground Health Checking]
--
--   If we idly time out,
--     ==> [Disconnected]
--
--     TODO Should't reject requests while disconnecting tue to idle timeout
--
-- [Reconnecting]
--   We intend to disconnect, then reconnect.
--
--   After disconnecting,
--     ==> [Connecting]

module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , EventHandlers(..)
  , createManagedBus
  , exchange
  , stream
  ) where

import Libriak.Connection (ConnectException, ConnectionError, Endpoint,
                           Interruptibility(..))
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..))
import RiakSTM            (TCounter, decrTCounter, incrTCounter, newTCounter,
                           readTCounter, registerOneShotEvent)

import qualified RiakBus   as Bus
import qualified RiakDebug as Debug

import Control.Concurrent.STM
import Control.Exception.Safe (throwIO, tryAny, tryAsync, uninterruptibleMask)
import Control.Foldl          (FoldM)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits           (KnownNat)
-- import System.Mem.Weak        (Weak, deRefWeak)


data ManagedBus
  = ManagedBus
  { uuid :: !Int
  , endpoint :: !Endpoint
  , healthCheckInterval :: !Int -- Microseconds
  , idleTimeout :: !Int -- Microseconds
  , receiveTimeout :: !Int -- Microseconds
  , statusVar :: !(TVar Status)
  , inFlightVar :: !TCounter
  , lastUsedRef :: !(IORef Word64)
    -- ^ The last time the bus was used.
  , handlers :: !EventHandlers
  , aliveRef :: !(IORef ())
    -- ^ Used for finalization.
  }

-- Managed bus, sans canary IORef.
data ManagedBus_
  = ManagedBus_
  { uuid :: !Int
  , endpoint :: !Endpoint
  , healthCheckInterval :: !Int
  , idleTimeout :: !Int
  , receiveTimeout :: !Int
  , statusVar :: !(TVar Status)
  , inFlightVar :: !TCounter
  , lastUsedRef :: !(IORef Word64)
  , handlers :: !EventHandlers
  }

data Status :: Type where
  Status :: !Word64 -> !State -> Status
  deriving stock (Eq)

data State :: Type where
  Disconnected :: State
  Connected :: !Bus -> !Bool -> State
  Blackhole :: State
  deriving stock (Eq)

data ManagedBusError :: Type where
  -- | The bus timed out waiting to become ready to accept requests.
  ManagedBusTimeoutError :: ManagedBusError
  ManagedBusPipelineError :: ManagedBusError
  -- | A connection error occurred during a send or receive.
  ManagedBusConnectionError :: !ConnectionError -> ManagedBusError
  -- | A protobuf decode error occurred.
  ManagedBusDecodeError :: !DecodeError -> ManagedBusError
  deriving stock (Show)

data EventHandlers
  = EventHandlers
  { onSend :: !(forall code. Request code -> IO ())
    -- ^ Called just prior to sending a request.
  , onReceive :: !(forall code. Response code -> IO ())
    -- ^ Called just after receiving a response.
  , onConnectError :: !(ConnectException 'Uninterruptible -> IO ())
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
     Int
  -> Endpoint
  -> Int -- ^ Health check interval (microseconds)
  -> Int -- ^ Idle timeout (microseconds)
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO ManagedBus
createManagedBus
    uuid endpoint healthCheckInterval idleTimeout receiveTimeout handlers = do

  statusVar :: TVar Status <-
    newTVarIO (Status 0 Disconnected)

  inFlightVar :: TCounter <-
    newTCounter

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  aliveRef :: IORef () <-
    newIORef ()

  pure ManagedBus
    { uuid = uuid
    , endpoint = endpoint
    , healthCheckInterval = healthCheckInterval
    , idleTimeout = idleTimeout
    , receiveTimeout = receiveTimeout
    , statusVar = statusVar
    , inFlightVar = inFlightVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    , aliveRef = aliveRef
    }

exchange ::
     KnownNat code
  => ManagedBus -- ^
  -> Int -- ^ Timeout (microseconds)
  -> Request code -- ^
  -> IO (Either ManagedBusError (Either (Response 0) (Response code)))
exchange managedBus timeout request =
  withBus managedBus timeout $ \bus ->
    Bus.exchange bus request

stream ::
     forall code r.
     KnownNat code
  => ManagedBus -- ^
  -> Int -- ^ Timeout (microseconds)
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either ManagedBusError (Either (Response 0) r))
stream managedBus timeout request responseFold =
  withBus managedBus timeout $ \bus ->
    Bus.stream bus request responseFold

withBus ::
     forall a.
     ManagedBus
  -> Int
     -- ^ Timeout (microseconds)
  -> (Bus -> IO (Either BusError a))
     -- ^ Bus callback, which may not throw an exception
  -> IO (Either ManagedBusError a)
withBus
    managedBus@(ManagedBus { inFlightVar, lastUsedRef, statusVar, uuid })
    timeout callback =

  uninterruptibleMask $ \unmask ->
    join . atomically $ do
      readTVar statusVar >>= \case
        -- Disconnected: if we successfully CAS a blackhole into the status var,
        -- fork a background thread to connect.
        Status generation Disconnected ->
          pure $ do
            maybeConnect managedBus_ generation
            unmask wait

        -- Connected and healthy: record the last used time, perform the
        -- callback with the healthy bus, and if it failed, if we successfully
        -- CAS a blackhole into the status var, fork a background thread to
        -- disconnect and reconnect.
        Status generation (Connected bus True) -> do
          incrTCounter inFlightVar
          pure (withHealthyBus unmask generation bus)

        -- Blackhole (connecting/disconnecting) or connected but unhealthy
        _ ->
          pure (unmask wait)

  where
    -- Perform an action with a healthy bus. This function is called with
    -- asynchronous exceptions masked, and the in-flight count has already been
    -- bumped.
    withHealthyBus ::
         (forall x. IO x -> IO x)
      -> Word64
      -> Bus
      -> IO (Either ManagedBusError a)
    withHealthyBus unmask generation bus = do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      result :: Either SomeException (Either BusError a) <-
        tryAsync (unmask (callback bus))

      atomically (decrTCounter inFlightVar)

      case result of
        Left ex -> do
          blackholeIfConnected
            statusVar
            generation
            (\bus ->
              void . forkIO $ do
                debug uuid generation ("thread died: " ++ show ex)
                drainAndDisconnect inFlightVar bus
                debug uuid generation "disconnected, reconnecting"
                connect managedBus_ (generation+1))

          throwIO ex

        Right (Left err) -> do
          blackholeIfConnected
            statusVar
            generation
            (\bus ->
              void . forkIO $ do
                debug uuid generation ("request failed: " ++ show err)
                drainAndDisconnect inFlightVar bus
                debug uuid generation "disconnected, reconnecting"
                connect managedBus_ (generation+1))

          unmask $ do
            -- Wait for us to transition off of a healthy status before
            -- returning, so that the client may immediately retry (and hit a
            -- 'wait', rather than this same code path)
            atomically $
              readTVar statusVar >>= \case
                Status generation' (Connected _ True) | generation == generation' ->
                  retry
                _ ->
                  pure ()

            pure (Left (fromBusError err))

        Right (Right result) ->
          pure (Right result)

    managedBus_ :: ManagedBus_
    managedBus_ =
      case managedBus of
        ManagedBus{..} ->
          ManagedBus_{..}

    wait :: IO (Either ManagedBusError a)
    wait = do
      timedOut :: STM () <-
        registerOneShotEvent timeout

      join . atomically $
        (pure (Left ManagedBusTimeoutError) <$ timedOut)
        <|>
        (readTVar statusVar >>= \case
          Status _ (Connected _ True) ->
            pure (withBus managedBus timeout callback)
          Status _ Disconnected ->
            pure (withBus managedBus timeout callback)
          _ ->
            retry)

    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusClosedError -> ManagedBusPipelineError
      BusConnectionError err -> ManagedBusConnectionError err
      BusDecodeError err -> ManagedBusDecodeError err

maybeConnect ::
     ManagedBus_
  -> Word64
  -> IO ()
maybeConnect bus@(ManagedBus_ { statusVar, uuid }) expectedGen =
  join . atomically $
    readTVar statusVar >>= \case
      Status actualGen Disconnected | actualGen == expectedGen -> do
        writeTVar statusVar (Status actualGen Blackhole)

        pure . void . forkIO $ do
          debug uuid expectedGen "connecting"
          connect bus expectedGen

      _ ->
        pure (pure ())

blackholeIfConnected ::
     TVar Status
  -> Word64
  -> (Bus -> IO ()) -- Action to call if we blackholed
  -> IO ()
blackholeIfConnected statusVar expectedGen action =
  join . atomically $
    readTVar statusVar >>= \case
      Status actualGen (Connected bus _) | actualGen == expectedGen -> do
        writeTVar statusVar (Status actualGen Blackhole)
        pure (action bus)

      _ ->
        pure (pure ())

-- TODO give up if bus gc'd
--
-- Precondition: statusVar contains 'Status generation Blackhole'
connect ::
     ManagedBus_
  -> Word64
  -> IO ()
connect
    managedBus@(ManagedBus_ { endpoint, handlers, healthCheckInterval,
                              idleTimeout, receiveTimeout, statusVar, uuid })
    generation =

  connectLoop generation 1

  where
    connectLoop ::
         Word64
      -> NominalDiffTime
      -> IO ()
    connectLoop generation seconds =
      Bus.connect endpoint receiveTimeout busHandlers >>= \case
        Left err -> do
          void (tryAny (onConnectError handlers err))
          debug uuid generation (show err ++ ", reconnecting in " ++ show seconds)
          sleep seconds
          connectLoop generation (seconds * 1.5)

        Right bus -> do
          debug uuid generation "connected, pinging until healthy"
          pingLoop bus generation seconds

    pingLoop :: Bus -> Word64 -> NominalDiffTime -> IO ()
    pingLoop bus generation seconds = do
      Bus.ping bus >>= \case
        Left err -> do
          debug uuid generation $
            show err ++ ", reconnecting in " ++ show seconds
          void (Bus.disconnect bus)
          sleep seconds
          connectLoop generation (seconds * 1.5)

        Right (Left err) -> do
          debug uuid generation (show err ++ ", pinging in " ++ show seconds)
          sleep seconds
          pingLoop bus generation (seconds * 1.5)

        Right (Right _) -> do
          atomically
            (writeTVar statusVar (Status generation (Connected bus True)))

          when (healthCheckInterval > 0)
            (void (forkIO (monitorHealth managedBus generation)))

          when (idleTimeout > 0)
            (void (forkIO (monitorUsage managedBus generation)))

    busHandlers :: Bus.EventHandlers
    busHandlers =
      Bus.EventHandlers
        { Bus.onSend = onSend handlers
        , Bus.onReceive = onReceive handlers
        , Bus.onConnectionError = onConnectionError handlers
        }

monitorHealth ::
     ManagedBus_
  -> Word64
  -> IO ()
monitorHealth
     managedBus@(ManagedBus_ { healthCheckInterval, inFlightVar, statusVar,
                               uuid })
     expectedGen = do

  debug uuid expectedGen "healthy"
  monitorLoop

  where
    monitorLoop :: IO ()
    monitorLoop = do
      threadDelay healthCheckInterval

      join . atomically $
        readTVar statusVar >>= \case
          Status actualGen (Connected bus True) | actualGen == expectedGen -> do
            incrTCounter inFlightVar
            pure $ do
              result :: Either BusError (Either (Response 0) (Response 2)) <-
                Bus.ping bus

              atomically (decrTCounter inFlightVar)

              case result of
                Left err ->
                  blackholeIfConnected statusVar expectedGen $ \bus ->  do
                    debug uuid expectedGen ("health check failed: " ++ show err)
                    drainAndDisconnect inFlightVar bus
                    debug uuid expectedGen "disconnected, reconnecting"
                    connect managedBus (expectedGen+1)

                Right (Left err) -> do
                  maybePingLoop err

                Right (Right _) ->
                  monitorLoop

          _ ->
            pure (pure ())

    maybePingLoop :: Response 0 -> IO ()
    maybePingLoop err =
      join . atomically $
        readTVar statusVar >>= \case
          Status actualGen (Connected bus True) | actualGen == expectedGen -> do
            writeTVar statusVar (Status actualGen (Connected bus False))

            pure $ do
              debug uuid expectedGen $
                "health check failed: " ++ show err ++ ", pinging until healthy"
              pingLoop 1

          _ ->
            pure (pure ())

    pingLoop :: NominalDiffTime -> IO ()
    pingLoop seconds = do
      sleep seconds

      join . atomically $
        readTVar statusVar >>= \case
          Status actualGen (Connected bus False) | actualGen == expectedGen -> do
            incrTCounter inFlightVar

            pure $ do
              result :: Either BusError (Either (Response 0) (Response 2)) <-
                Bus.ping bus

              atomically (decrTCounter inFlightVar)

              case result of
                Left err ->
                  blackholeIfConnected statusVar expectedGen $ \bus -> do
                    debug uuid expectedGen ("ping failed: " ++ show err)
                    drainAndDisconnect inFlightVar bus
                    debug uuid expectedGen "disconnected, reconnecting"
                    connect managedBus (expectedGen+1)

                Right (Left err) -> do
                  debug uuid expectedGen $
                    "ping failed: " ++ show err ++ ", retrying in " ++ show seconds
                  pingLoop (seconds * 1.5)

                Right (Right _) ->
                  maybeMonitorLoop

          _ ->
            pure (pure ())

    maybeMonitorLoop :: IO ()
    maybeMonitorLoop =
      join . atomically $
        readTVar statusVar >>= \case
          Status actualGen (Connected bus False) | actualGen == expectedGen -> do
            writeTVar statusVar (Status actualGen (Connected bus True))

            pure $ do
              debug uuid expectedGen "healthy"
              monitorLoop

          _ ->
            pure (pure ())

monitorUsage ::
     ManagedBus_
  -> Word64
  -> IO ()
monitorUsage
    (ManagedBus_ { idleTimeout, inFlightVar, lastUsedRef, statusVar, uuid })
    expectedGen =

  loop

  where
    loop :: IO ()
    loop = do
      timer :: STM () <-
        registerOneShotEvent (idleTimeout `div` 2)

      join . atomically $
        (timer $> handleTimer)
        <|>
        (readTVar statusVar >>= \case
            Status actualGen state | actualGen == expectedGen ->
              case state of
                Connected _ True ->
                  retry
                Connected _ False ->
                  pure handleUnhealthy
                _ ->
                  pure (pure ())
            _ ->
              pure (pure ()))

      where
        handleTimer :: IO ()
        handleTimer = do
          now <- getMonotonicTimeNSec
          lastUsed <- readIORef lastUsedRef

          if now - lastUsed > fromIntegral (idleTimeout * 1000)
            then
              blackholeIfConnected statusVar expectedGen $ \bus -> do
                debug uuid expectedGen "idle timeout"
                drainAndDisconnect inFlightVar bus
                debug uuid expectedGen "disconnected"
                atomically
                  (writeTVar statusVar (Status (expectedGen+1) Disconnected))
            else
              loop

        -- Don't count an unhealthy socket against its idle timeout time,
        -- because requests cannot be made with it (so its lastUsed timestamp is
        -- static).
        handleUnhealthy :: IO ()
        handleUnhealthy =
          join . atomically $
            readTVar statusVar >>= \case
              Status actualGen state | actualGen == expectedGen ->
                case state of
                  Connected _ True ->
                    pure loop
                  Connected _ False ->
                    retry
                  _ ->
                    pure (pure ())
              _ ->
                pure (pure ())

drainAndDisconnect ::
     TCounter
  -> Bus
  -> IO ()
drainAndDisconnect inFlightVar bus = do
  atomically $
    readTCounter inFlightVar >>= \case
      0 -> pure ()
      _ -> retry

  void (Bus.disconnect bus)

debug :: Int -> Word64 -> [Char] -> IO ()
debug uuid gen msg =
  Debug.debug ("handle " ++ show uuid ++ "." ++ show gen ++ ": " ++ msg)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
