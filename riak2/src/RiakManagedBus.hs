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
  , withBus
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..))

import qualified RiakBus   as Bus
import qualified RiakDebug as Debug

import Control.Concurrent.STM
import Control.Exception.Safe (tryAny, uninterruptibleMask_)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
-- import System.Mem.Weak        (Weak, deRefWeak)


data ManagedBus
  = ManagedBus
  { uuid :: !Int
  , endpoint :: !Endpoint
  , receiveTimeout :: !Int
  , statusVar :: !(TVar Status)
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
  , receiveTimeout :: !Int
  , statusVar :: !(TVar Status)
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
     Int
  -> Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO ManagedBus
createManagedBus uuid endpoint receiveTimeout handlers = do
  statusVar :: TVar Status <-
    newTVarIO (Status 0 Disconnected)

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  aliveRef :: IORef () <-
    newIORef ()

  pure ManagedBus
    { uuid = uuid
    , endpoint = endpoint
    , receiveTimeout = receiveTimeout
    , statusVar = statusVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    , aliveRef = aliveRef
    }

withBus ::
     forall a.
     ManagedBus
  -> Int -- Microsecond wait
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus managedBus@(ManagedBus { lastUsedRef, statusVar }) timeout callback =
  readTVarIO statusVar >>= \case
    -- Disconnected: if we successfully CAS a blackhole into the status var,
    -- fork a background thread to connect.
    Status generation Disconnected -> do
      maybeConnect managedBus_ generation
      wait

    -- Connected and healthy: record the last used time, perform the callback
    -- with the healthy bus, and if it failed, if we successfully CAS a
    -- blackhole into the status var, fork a background thread to disconnect and
    -- reconnect.
    Status generation (Connected bus True) -> do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      callback bus >>= \case
        -- FIXME we must disconnect if callback throws an exception, but can
        -- it?
        Left err -> do
          uninterruptibleMask_ $
            blackholeIfConnected
              statusVar
              generation
              (\bus ->
                void (forkIO (disconnect managedBus_ bus True)))

          -- Wait for us to transition off of a healthy status before returning,
          -- so that the client may immediately retry (and hit a 'wait', rather
          -- than this same code path)
          atomically $
            readTVar statusVar >>= \case
              Status generation' (Connected _ True) | generation == generation' ->
                retry
              _ ->
                pure ()

          pure (Left (fromBusError err))

        Right result ->
          pure (Right result)

    -- Blackhole (connecting/disconnecting) or connected but unhealthy
    _ ->
      wait

  where
    managedBus_ :: ManagedBus_
    managedBus_ =
      case managedBus of
        ManagedBus{..} ->
          ManagedBus_{..}

    wait :: IO (Either ManagedBusError a)
    wait = do
      timeoutVar :: TVar Bool <-
        registerDelay timeout

      join . atomically $
        (readTVar timeoutVar >>= \case
          False ->
            retry
          True ->
            pure (pure (Left ManagedBusTimeoutError)))
        <|>
        (readTVar statusVar >>= \case
          Status _ (Connected _ True) ->
            -- TODO don't call withBus, just jump into correct branch
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
  uninterruptibleMask_ . join . atomically $
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
    managedBus@(ManagedBus_ { endpoint, handlers, receiveTimeout, statusVar,
                              uuid })
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

          void (forkIO (monitorHealth managedBus generation))
          -- void (forkIO (idleTimeout managedBus))

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
monitorHealth managedBus@(ManagedBus_ { statusVar, uuid }) expectedGen = do
  debug uuid expectedGen "healthy"
  monitorLoop

  where
    monitorLoop :: IO ()
    monitorLoop = do
      -- TODO configurable ping frequency
      -- threadDelay (1*1000*1000)
      threadDelay (125*1000)

      readTVarIO statusVar >>= \case
        Status actualGen (Connected bus True) | actualGen == expectedGen -> do
          Bus.ping bus >>= \case
            Left err ->
              blackholeIfConnected statusVar expectedGen $ \bus ->  do
                debug uuid expectedGen ("health check failed: " ++ show err)
                disconnect managedBus bus True

            Right (Left err) -> do
              maybePingLoop err

            Right (Right _) ->
              monitorLoop

        _ ->
          pure ()

    maybePingLoop :: Response 0 -> IO ()
    maybePingLoop err =
      join . atomically $ do
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

      readTVarIO statusVar >>= \case
        Status actualGen (Connected bus False) | actualGen == expectedGen ->
          Bus.ping bus >>= \case
            Left err -> do
              debug uuid expectedGen ("ping failed: " ++ show err)

              blackholeIfConnected statusVar expectedGen $ \bus ->
                disconnect managedBus bus True

            Right (Left err) -> do
              debug uuid expectedGen $
                "ping failed: " ++ show err ++ ", retrying in " ++ show seconds
              pingLoop (seconds * 1.5)

            Right (Right _) -> do
              debug uuid expectedGen "healthy"
              monitorLoop

        _ ->
          pure ()

{-
idleTimeout ::
     ManagedBus_
  -> IO ()
idleTimeout bus@(ManagedBus_ { lastUsedRef, statusVar }) =
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
              undefined
              -- join . atomically $
              --   readTVar inFlightVar >>= \case
              --     -- We have no in-flight requests, and the idle timeout has
              --     -- elapsed.
              --     0 -> do
              --       pure (disconnect bus False)

              --     -- We have an in-flight request, so even though the idle
              --     -- timeout has elapsed, just pretend it hasn't.
              --     _ ->
              --       pure loop
            else
              loop

        Disconnected -> pure ()
        Disconnecting -> pure ()
        Blackhole -> pure ()
-}

disconnect ::
     ManagedBus_
  -> Bus
  -> Bool
  -> IO ()
disconnect managedBus@(ManagedBus_ { statusVar, uuid }) bus reconnectAfter = do
  Status generation Blackhole <-
    readTVarIO statusVar

  atomically $
    Bus.inFlight bus >>= \case
      0 -> pure ()
      _ -> retry

  void (Bus.disconnect bus)

  if reconnectAfter
    then do
      debug uuid generation "reconnecting"
      connect managedBus (generation+1)
    else do
      debug uuid generation "disconnected"
      atomically (writeTVar statusVar (Status (generation+1) Disconnected))

debug :: Int -> Word64 -> [Char] -> IO ()
debug uuid gen msg =
  Debug.debug ("handle " ++ show uuid ++ "." ++ show gen ++ ": " ++ msg)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
