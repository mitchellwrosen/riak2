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
  , ManagedBusConfig(..)
  , EventHandlers(..)
  , ManagedBusError(..)
  , createManagedBus
    -- * API
  , delete
  , deleteIndex
  , get
  , getBucket
  , getBucketType
  , getCrdt
  , getIndex
  , getSchema
  , getServerInfo
  -- , listBuckets
  -- , listKeys
  -- , mapReduce
  , ping
  , put
  , putIndex
  , putSchema
  , resetBucket
  , search
  -- , secondaryIndex
  , setBucket
  , setBucketType
  , updateCrdt

  , stream
  ) where

import Libriak.Connection (ConnectException, ConnectionError, Endpoint,
                           Interruptibility(..))
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakSTM            (TCounter, decrTCounter, incrTCounter, newTCounter,
                           readTCounter, registerOneShotEvent)

import qualified Libriak.Handle as Handle
import qualified RiakDebug      as Debug

import Control.Concurrent.STM
import Control.Exception.Safe (throwIO, tryAny, tryAsync, uninterruptibleMask)
import Control.Foldl          (FoldM)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits           (KnownNat)
-- import System.Mem.Weak        (Weak, deRefWeak)

import qualified Data.Riak.Proto as Proto

-- TODO configure connecting wait time
waitTimeout :: Int
waitTimeout = 5*1000*1000

data ManagedBus
  = ManagedBus
  { uuid :: !Int
  , endpoint :: !Endpoint
  , healthCheckInterval :: !Int
  , idleTimeout :: !Int
  , receiveTimeout :: !Int
  , handlers :: !EventHandlers

  , statusVar :: !(TVar Status)
  , inFlightVar :: !TCounter
  , lastUsedRef :: !(IORef Word64)
    -- ^ The last time the bus was used.
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
  Connected :: !Handle.Handle -> !Bool -> State
  Blackhole :: State
  deriving stock (Eq)

data ManagedBusConfig
  = ManagedBusConfig
  { uuid :: !Int
  , endpoint :: !Endpoint
  , healthCheckInterval :: !Int -- Microseconds
  , idleTimeout :: !Int -- Microseconds
  , receiveTimeout :: !Int -- Microseconds
  , handlers :: !EventHandlers
  }

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
     ManagedBusConfig
  -> IO ManagedBus
createManagedBus
    ManagedBusConfig { endpoint, handlers, healthCheckInterval, idleTimeout,
                       receiveTimeout, uuid
                     } = do

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

withHandle ::
     forall a.
     ManagedBus
  -> (Handle.Handle -> IO (Either Handle.HandleError a))
  -> IO (Either ManagedBusError a)
withHandle
    managedBus@(ManagedBus { inFlightVar, lastUsedRef, statusVar, uuid })
    callback =

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
        -- callback with the healthy handle, and if it failed, if we
        -- successfully CAS a blackhole into the status var, fork a background
        -- thread to disconnect and reconnect.
        Status generation (Connected handle True) -> do
          incrTCounter inFlightVar
          pure (withHealthyHandle unmask generation handle)

        -- Blackhole (connecting/disconnecting) or connected but unhealthy
        _ ->
          pure (unmask wait)

  where
    -- Perform an action with a healthy handle. This function is called with
    -- asynchronous exceptions masked, and the in-flight count has already been
    -- bumped.
    withHealthyHandle ::
         (forall x. IO x -> IO x)
      -> Word64
      -> Handle.Handle
      -> IO (Either ManagedBusError a)
    withHealthyHandle unmask generation handle = do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      result :: Either SomeException (Either Handle.HandleError a) <-
        tryAsync (unmask (callback handle))

      atomically (decrTCounter inFlightVar)

      case result of
        Left ex -> do
          blackholeIfConnected
            statusVar
            generation
            (\handle ->
              void . forkIO $ do
                debug uuid generation ("thread died: " ++ show ex)
                drainAndDisconnect inFlightVar handle
                debug uuid generation "disconnected, reconnecting"
                connect managedBus_ (generation+1))

          throwIO ex

        Right (Left err) -> do
          blackholeIfConnected
            statusVar
            generation
            (\handle ->
              void . forkIO $ do
                debug uuid generation ("request failed: " ++ show err)
                drainAndDisconnect inFlightVar handle
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

            pure (Left (fromHandleError err))

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
        registerOneShotEvent waitTimeout

      join . atomically $
        (pure (Left ManagedBusTimeoutError) <$ timedOut)
        <|>
        (readTVar statusVar >>= \case
          Status _ (Connected _ True) ->
            pure (withHandle managedBus callback)
          Status _ Disconnected ->
            pure (withHandle managedBus callback)
          _ ->
            retry)

    fromHandleError :: Handle.HandleError -> ManagedBusError
    fromHandleError = \case
      Handle.HandleClosedError -> ManagedBusPipelineError
      Handle.HandleConnectionError err -> ManagedBusConnectionError err
      Handle.HandleDecodeError err -> ManagedBusDecodeError err

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
  -> (Handle.Handle -> IO ()) -- Action to call if we blackholed
  -> IO ()
blackholeIfConnected statusVar expectedGen action =
  join . atomically $
    readTVar statusVar >>= \case
      Status actualGen (Connected handle _) | actualGen == expectedGen -> do
        writeTVar statusVar (Status actualGen Blackhole)
        pure (action handle)

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
      Handle.connect endpoint receiveTimeout handleHandlers >>= \case
        Left err -> do
          void (tryAny (onConnectError handlers err))
          debug uuid generation (show err ++ ", reconnecting in " ++ show seconds)
          sleep seconds
          connectLoop generation (seconds * 1.5)

        Right handle -> do
          debug uuid generation "connected, pinging until healthy"
          pingLoop handle generation seconds

    pingLoop :: Handle.Handle -> Word64 -> NominalDiffTime -> IO ()
    pingLoop handle generation seconds = do
      Handle.ping handle >>= \case
        Left err -> do
          debug uuid generation $
            show err ++ ", reconnecting in " ++ show seconds
          void (Handle.disconnect handle)
          sleep seconds
          connectLoop generation (seconds * 1.5)

        Right (Left err) -> do
          debug uuid generation (show err ++ ", pinging in " ++ show seconds)
          sleep seconds
          pingLoop handle generation (seconds * 1.5)

        Right (Right _) -> do
          atomically
            (writeTVar statusVar (Status generation (Connected handle True)))

          when (healthCheckInterval > 0)
            (void (forkIO (monitorHealth managedBus generation)))

          when (idleTimeout > 0)
            (void (forkIO (monitorUsage managedBus generation)))

    handleHandlers :: Handle.EventHandlers
    handleHandlers =
      Handle.EventHandlers
        { Handle.onSend = onSend handlers
        , Handle.onReceive = onReceive handlers
        , Handle.onError = mempty -- FIXME
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
          Status actualGen (Connected handle True) | actualGen == expectedGen -> do
            incrTCounter inFlightVar
            pure $ do
              result :: Either Handle.HandleError (Either ByteString ()) <-
                Handle.ping handle

              atomically (decrTCounter inFlightVar)

              case result of
                Left err ->
                  blackholeIfConnected statusVar expectedGen $ \handle ->  do
                    debug uuid expectedGen ("health check failed: " ++ show err)
                    drainAndDisconnect inFlightVar handle
                    debug uuid expectedGen "disconnected, reconnecting"
                    connect managedBus (expectedGen+1)

                Right (Left err) -> do
                  maybePingLoop err

                Right (Right _) ->
                  monitorLoop

          _ ->
            pure (pure ())

    maybePingLoop :: ByteString -> IO ()
    maybePingLoop err =
      join . atomically $
        readTVar statusVar >>= \case
          Status actualGen (Connected handle True) | actualGen == expectedGen -> do
            writeTVar statusVar (Status actualGen (Connected handle False))

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
          Status actualGen (Connected handle False) | actualGen == expectedGen -> do
            incrTCounter inFlightVar

            pure $ do
              result :: Either Handle.HandleError (Either ByteString ()) <-
                Handle.ping handle

              atomically (decrTCounter inFlightVar)

              case result of
                Left err ->
                  blackholeIfConnected statusVar expectedGen $ \handle -> do
                    debug uuid expectedGen ("ping failed: " ++ show err)
                    drainAndDisconnect inFlightVar handle
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
          Status actualGen (Connected handle False) | actualGen == expectedGen -> do
            writeTVar statusVar (Status actualGen (Connected handle True))

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
              blackholeIfConnected statusVar expectedGen $ \handle -> do
                debug uuid expectedGen "idle timeout"
                drainAndDisconnect inFlightVar handle
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
  -> Handle.Handle
  -> IO ()
drainAndDisconnect inFlightVar handle = do
  atomically $
    readTCounter inFlightVar >>= \case
      0 -> pure ()
      _ -> retry

  void (Handle.disconnect handle)

debug :: Int -> Word64 -> [Char] -> IO ()
debug uuid gen msg =
  Debug.debug ("handle " ++ show uuid ++ "." ++ show gen ++ ": " ++ msg)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))

--------------------------------------------------------------------------------
-- Libriak.Handle wrappers
--------------------------------------------------------------------------------

delete ::
     ManagedBus -- ^
  -> Proto.RpbDelReq
  -> IO (Either ManagedBusError (Either ByteString ()))
delete bus request =
  withHandle bus $ \handle ->
    Handle.delete handle request

deleteIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either ManagedBusError (Either ByteString ()))
deleteIndex bus request =
  withHandle bus $ \handle ->
    Handle.deleteIndex handle request

get ::
     ManagedBus
  -> Proto.RpbGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetResp))
get bus request =
  withHandle bus $ \handle ->
    Handle.get handle request

getBucket ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetBucketResp))
getBucket bus request =
  withHandle bus $ \handle ->
    Handle.getBucket handle request

getBucketType ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetBucketResp))
getBucketType bus request =
  withHandle bus $ \handle ->
    Handle.getBucketType handle request

getCrdt ::
     ManagedBus
  -> Proto.DtFetchReq
  -> IO (Either ManagedBusError (Either ByteString Proto.DtFetchResp))
getCrdt bus request =
  withHandle bus $ \handle ->
    Handle.getCrdt handle request

getIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex bus request =
  withHandle bus $ \handle ->
    Handle.getIndex handle request

getSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema bus request =
  withHandle bus $ \handle ->
    Handle.getSchema handle request

getServerInfo ::
     ManagedBus
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo bus =
  withHandle bus Handle.getServerInfo

-- listBuckets ::
--      Handle
--   -> Proto.RpbListBucketsReq
--   -> FoldM IO (Response 16) r
--   -> IO (Either HandleError (Either (Response 0) r))
-- listBuckets handle request =
--   stream handle (ReqRpbListBuckets request)

-- listKeys ::
--      Handle
--   -> Proto.RpbListKeysReq
--   -> FoldM IO (Response 18) r
--   -> IO (Either HandleError (Either (Response 0) r))
-- listKeys handle request =
--   stream handle (ReqRpbListKeys request)

-- mapReduce ::
--      Handle
--   -> Proto.RpbMapRedReq
--   -> FoldM IO (Response 24) r
--   -> IO (Either HandleError (Either (Response 0) r))
-- mapReduce handle request =
--   stream handle (ReqRpbMapRed request)

ping ::
     ManagedBus
  -> IO (Either ManagedBusError (Either ByteString ()))
ping bus =
  withHandle bus Handle.ping

put ::
     ManagedBus
  -> Proto.RpbPutReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbPutResp))
put bus request =
  withHandle bus $ \handle ->
    Handle.put handle request

putIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either ManagedBusError (Either ByteString ()))
putIndex bus request =
  withHandle bus $ \handle ->
    Handle.putIndex handle request

putSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either ManagedBusError (Either ByteString ()))
putSchema bus request =
  withHandle bus $ \handle ->
    Handle.putSchema handle request

resetBucket ::
     ManagedBus
  -> Proto.RpbResetBucketReq
  -> IO (Either ManagedBusError (Either ByteString ()))
resetBucket bus request =
  withHandle bus $ \handle ->
    Handle.resetBucket handle request

setBucket ::
     ManagedBus
  -> Proto.RpbSetBucketReq
  -> IO (Either ManagedBusError (Either ByteString ()))
setBucket bus request =
  withHandle bus $ \handle ->
    Handle.setBucket handle request

setBucketType ::
     ManagedBus
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either ManagedBusError (Either ByteString ()))
setBucketType bus request =
  withHandle bus $ \handle ->
    Handle.setBucketType handle request

search ::
     ManagedBus
  -> Proto.RpbSearchQueryReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbSearchQueryResp))
search bus request =
  withHandle bus $ \handle ->
    Handle.search handle request

-- secondaryIndex ::
--      Handle
--   -> Proto.RpbIndexReq
--   -> FoldM IO (Response 26) r
--   -> IO (Either HandleError (Either (Response 0) r))
-- secondaryIndex handle request =
--   stream handle (ReqRpbIndex request)

updateCrdt ::
     ManagedBus -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.DtUpdateResp))
updateCrdt bus request =
  withHandle bus $ \handle ->
    Handle.updateCrdt handle request

stream ::
     forall code r.
     KnownNat code
  => ManagedBus -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either ManagedBusError (Either ByteString r))
stream managedBus request responseFold =
  withHandle managedBus $ \handle ->
    Handle.stream handle request responseFold
