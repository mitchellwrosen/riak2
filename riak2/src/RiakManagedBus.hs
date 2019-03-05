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

import qualified RiakBus as Bus
import qualified RiakDebug as Debug

import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, tryAny)
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
  Disconnecting :: State
  Connecting :: State
  Unhealthy :: !Bus -> State
  Healthy :: !Bus -> State
  deriving stock (Eq)

showState :: State -> [Char]
showState = \case
  Disconnected -> "disconnected"
  Disconnecting -> "disconnecting"
  Connecting -> "connecting"
  Unhealthy _ -> "unhealthy"
  Healthy _ -> "healthy"

data ManagedBusError :: Type where
  -- | The bus is not ready to accept requests, either because it is connecting,
  -- unhealthy, or draining connections and about to reconnect.
  ManagedBusDisconnectedError :: ManagedBusError
  -- | A connection error occurred during a send or receive.
  ManagedBusConnectionError :: !ConnectionError -> ManagedBusError
  -- | A protobuf decode error occurred.
  ManagedBusDecodeError :: !DecodeError -> ManagedBusError
  deriving stock (Show)

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

-- | An STM action that returns when the managed bus is connected and healthy.
managedBusReady :: ManagedBus -> STM ()
managedBusReady ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Status _ Disconnected -> retry
    Status _ Disconnecting -> retry
    Status _ Connecting -> retry
    Status _ (Unhealthy _) -> retry
    Status _ (Healthy _) -> pure ()

withBus ::
     forall a.
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus bus@(ManagedBus { lastUsedRef, statusVar, uuid }) callback = do
  Status generation state <-
    readTVarIO statusVar

  case state of
    Disconnected -> do
      void . forkIO $ do
        debug uuid generation "possibly connecting"
        connect bus_ generation

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

      callback bus >>= \case
        -- FIXME we must disconnect if callback throws an exception, but can
        -- it?
        Left err -> do
          void . forkIO $ do
            debug uuid generation "request failed, disconnecting"
            disconnect bus_ generation True

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


-- TODO give up if bus gc'd
connect ::
     ManagedBus_
  -> Word64
  -> IO ()
connect
    managedBus@(ManagedBus_ { endpoint, handlers, receiveTimeout, statusVar,
                              uuid })
    expectedGen =

  join . atomically $
    readTVar statusVar >>= \case
      Status actualGen state | actualGen == expectedGen ->
        case state of
          Disconnected -> do
            writeTVar statusVar (Status actualGen Connecting)

            pure $ do
              debug uuid expectedGen "connecting"
              connectLoop expectedGen 1

          _ ->
            pure $
              debug uuid expectedGen $
                "was going to connect, but handle is " ++ showState state

      Status actualGen _ ->
        pure $
          debug uuid expectedGen $
            "was going to connect, but found gen " ++ show actualGen

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
          debug uuid generation (show err ++ ", disconnecting")
          void (Bus.disconnect bus)
          debug uuid generation ("reconnecting in " ++ show seconds)
          sleep seconds
          connectLoop generation (seconds * 1.5)

        Right (Left err) -> do
          debug uuid generation (show err ++ ", pinging in " ++ show seconds)
          sleep seconds
          pingLoop bus generation (seconds * 1.5)

        Right (Right _) -> do
          debug uuid generation "healthy"

          atomically $ do
            status <- readTVar statusVar
            assert (status == Status generation Connecting) (pure ())
            writeTVar statusVar (Status generation (Healthy bus))

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
monitorHealth
    managedBus@(ManagedBus_ { statusVar, uuid }) expectedGen = do

  debug uuid expectedGen "monitoring health"
  monitorLoop

  where
    monitorLoop :: IO ()
    monitorLoop = do
      -- TODO configurable ping frequency
      -- threadDelay (1*1000*1000)
      threadDelay (125*1000)

      readTVarIO statusVar >>= \case
        Status actualGen (Healthy bus) | actualGen == expectedGen -> do
          Bus.ping bus >>= \case
            Left err -> do
              debug uuid expectedGen $
                "health check failed: " ++ show err ++ ", disconnecting"
              disconnect managedBus expectedGen True

            Right (Left err) -> do
              debug uuid expectedGen ("health check failed: " ++ show err)

              join . atomically $ do
                readTVar statusVar >>= \case
                  Status actualGen state | actualGen == expectedGen ->
                    case state of
                      Healthy bus -> do
                        writeTVar statusVar (Status actualGen (Unhealthy bus))
                        pure $ do
                          debug uuid expectedGen "pinging until healthy"
                          pingLoop 1

                      _ ->
                        pure $
                          debug uuid expectedGen $
                            "was going to ping until healthy, but handle " ++
                              "is " ++ showState state

                  Status actualGen _ ->
                    pure $
                      debug uuid expectedGen $
                        "was going to ping until healthy, but found gen " ++
                          show actualGen

            Right (Right _) ->
              monitorLoop

        _ ->
          pure ()

    pingLoop :: NominalDiffTime -> IO ()
    pingLoop seconds = do
      sleep seconds

      readTVarIO statusVar >>= \case
        Status actualGen state | actualGen == expectedGen ->
          case state of
            Unhealthy bus ->
              Bus.ping bus >>= \case
                Left err -> do
                  debug uuid expectedGen $
                    "ping failed: " ++ show err ++ ", disconnecting"
                  disconnect managedBus expectedGen True

                Right (Left err) -> do
                  debug uuid expectedGen $
                    "ping failed: " ++ show err ++ ", retrying in " ++ show seconds
                  pingLoop (seconds * 1.5)

                Right (Right _) -> do
                  debug uuid expectedGen "healthy"
                  monitorLoop

            _ ->
              debug uuid expectedGen $
                "was going to ping, but handle is " ++ showState state

        Status actualGen _ ->
          debug uuid expectedGen $
            "was going to ping, but found gen " ++ show actualGen

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
        Connecting -> pure ()
-}

disconnect ::
     ManagedBus_
  -> Word64
  -> Bool
  -> IO ()
disconnect
    managedBus@(ManagedBus_ { statusVar, uuid })
    expectedGen
    reconnectAfter =

  join . atomically $ do
    Status actualGen state <-
      readTVar statusVar

    if actualGen == expectedGen
      then
        case state of
          Unhealthy bus -> do
            writeTVar statusVar (Status actualGen Disconnecting)
            pure (doDisconnect bus actualGen)

          Healthy bus -> do
            writeTVar statusVar (Status actualGen Disconnecting)
            pure (doDisconnect bus actualGen)

          _ ->
            pure $
              debug uuid expectedGen $
                "was going to disconnect, but handle was " ++ showState state

      else
        pure $
          debug uuid expectedGen
            ("was going to disconnect, but found gen " ++ show actualGen)

  where
    doDisconnect :: Bus -> Word64 -> IO ()
    doDisconnect bus generation = do
      debug uuid generation "draining connections"

      atomically $
        Bus.inFlight bus >>= \case
          0 -> pure ()
          _ -> retry

      debug uuid generation "disconnecting"

      void (Bus.disconnect bus)

      atomically $ do
        status <- readTVar statusVar
        assert (status == Status generation Disconnecting) (pure ())
        writeTVar statusVar (Status (generation+1) Disconnected)

      debug uuid generation "disconnected"

      when reconnectAfter
        (connect managedBus (generation+1))

debug :: Int -> Word64 -> [Char] -> IO ()
debug uuid gen msg =
  Debug.debug ("handle " ++ show uuid ++ "." ++ show gen ++ ": " ++ msg)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
