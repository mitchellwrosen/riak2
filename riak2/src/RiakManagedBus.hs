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
  , listBuckets
  , listKeys
  , mapReduce
  , ping
  , put
  , putIndex
  , putSchema
  , resetBucket
  , search
  , secondaryIndex
  , setBucket
  , setBucketType
  , updateCrdt
  ) where

import Libriak.Connection (ConnectionError)
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
import Socket.Stream.IPv4     (ConnectException, Endpoint, Interruptibility(..))

import qualified Data.Riak.Proto as Proto

-- TODO configure connecting wait time
waitTimeout :: Int
waitTimeout = 5*1000*1000

data ManagedBus
  = ManagedBus
  { uuid :: Int
  , endpoint :: Endpoint
  , healthCheckInterval :: Int
  , idleTimeout :: Int
  , receiveTimeout :: Int
  , handlers :: EventHandlers

  , generationVar :: TVar Word64
  , stateVar :: TVar State
  , inFlightVar :: TCounter
  , lastUsedRef :: IORef Word64
    -- ^ The last time the bus was used.
  }

data State :: Type where
  Disconnected :: State
  Connecting :: State
  Connected :: Handle.Handle -> Health -> State
  Disconnecting :: State
  deriving stock (Eq)

data Health
  = Healthy
  | Unhealthy
  deriving stock (Eq)

data ManagedBusConfig
  = ManagedBusConfig
  { uuid :: Int
  , endpoint :: Endpoint
  , healthCheckInterval :: Int -- Microseconds
  , idleTimeout :: Int -- Microseconds
  , receiveTimeout :: Int -- Microseconds
  , handlers :: EventHandlers
  } deriving stock (Show)

data ManagedBusError :: Type where
  -- | The bus timed out waiting to become ready to accept requests.
  ManagedBusTimeoutError :: ManagedBusError
  ManagedBusPipelineError :: ManagedBusError
  -- | A connection error occurred during a send or receive.
  ManagedBusConnectionError :: ConnectionError -> ManagedBusError
  -- | A protobuf decode error occurred.
  ManagedBusDecodeError :: DecodeError -> ManagedBusError
  deriving stock (Show)

data EventHandlers
  = EventHandlers
  { onSend :: forall code. Request code -> IO ()
    -- ^ Called just prior to sending a request.
  , onReceive :: forall code. Response code -> IO ()
    -- ^ Called just after receiving a response.
  , onConnectError :: ConnectException 'Uninterruptible -> IO ()
  , onConnectionError :: ConnectionError -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 <> EventHandlers a2 b2 c2 d2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Show EventHandlers where
  show _ =
    "<EventHandlers>"

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

  generationVar :: TVar Word64 <-
    newTVarIO 0

  stateVar :: TVar State <-
    newTVarIO Disconnected

  inFlightVar :: TCounter <-
    newTCounter

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  pure ManagedBus
    { uuid = uuid
    , endpoint = endpoint
    , healthCheckInterval = healthCheckInterval
    , idleTimeout = idleTimeout
    , receiveTimeout = receiveTimeout
    , generationVar = generationVar
    , stateVar = stateVar
    , inFlightVar = inFlightVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    }

withHandle ::
     forall a.
     ManagedBus
  -> (Handle.Handle -> IO (Either Handle.HandleError a))
  -> IO (Either ManagedBusError a)
withHandle
    managedBus@(ManagedBus { generationVar, inFlightVar, lastUsedRef, stateVar,
                             uuid })
    callback =

  uninterruptibleMask $ \unmask ->
    atomicallyIO $ do
      readTVar stateVar >>= \case
        Disconnected -> do
          writeTVar stateVar Connecting

          pure $ do
            void (forkIO (connect managedBus))
            unmask wait

        Connected handle Healthy -> do
          incrTCounter inFlightVar
          generation <- readTVar generationVar
          pure (withHealthyHandle unmask generation handle)

        Connecting            -> pure (unmask wait)
        Connected _ Unhealthy -> pure (unmask wait)
        Disconnecting         -> pure (unmask wait)

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
          atomicallyIO $
            whenGen generation generationVar $
              readTVar stateVar >>= \case
                Connected handle _ -> do
                  writeTVar stateVar Disconnecting

                  pure . void . forkIO $ do
                    debug uuid generation ("thread died: " ++ show ex)

                    drainAndDisconnect inFlightVar handle

                    debug uuid generation "disconnected, reconnecting"

                    atomically $ do
                      modifyTVar' generationVar (+1)
                      writeTVar stateVar Connecting

                    connect managedBus

                Disconnecting ->
                  pure (pure ())

                Disconnected -> undefined
                Connecting   -> undefined

          throwIO ex

        Right (Left err) -> do
          atomicallyIO $
            whenGen generation generationVar $
              readTVar stateVar >>= \case
                Connected handle _ -> do
                  writeTVar stateVar Disconnecting

                  pure . void . forkIO $ do
                    debug uuid generation ("request failed: " ++ show err)

                    drainAndDisconnect inFlightVar handle

                    debug uuid generation "disconnected, reconnecting"

                    atomically $ do
                      modifyTVar' generationVar (+1)
                      writeTVar stateVar Connecting

                    connect managedBus

                Disconnecting ->
                  pure (pure ())

                Disconnected -> undefined
                Connecting -> undefined

          unmask $ do
            -- Wait for us to transition off of a healthy status before
            -- returning, so that the client may immediately retry (and hit a
            -- 'wait', rather than this same code path)
            atomically $ do
              actualGen <- readTVar generationVar
              if actualGen == generation
                then do
                  readTVar stateVar >>= \case
                    Connected _ Healthy -> retry
                    _ -> pure ()
                else
                  pure ()

            pure (Left (fromHandleError err))

        Right (Right result) ->
          pure (Right result)

    wait :: IO (Either ManagedBusError a)
    wait = do
      timedOut :: STM () <-
        registerOneShotEvent waitTimeout

      atomicallyIO $
        (pure (Left ManagedBusTimeoutError) <$ timedOut)
        <|>
        (readTVar stateVar >>= \case
          Connected _ Healthy ->
            pure (withHandle managedBus callback)
          Disconnected ->
            pure (withHandle managedBus callback)

          Connecting -> retry
          Connected _ Unhealthy -> retry
          Disconnecting -> retry)

    fromHandleError :: Handle.HandleError -> ManagedBusError
    fromHandleError = \case
      Handle.HandleClosedError -> ManagedBusPipelineError
      Handle.HandleConnectionError err -> ManagedBusConnectionError err
      Handle.HandleDecodeError err -> ManagedBusDecodeError err

-- TODO give up if bus gc'd
connect ::
     ManagedBus
  -> IO ()
connect
    managedBus@(ManagedBus { endpoint, generationVar, handlers,
                             healthCheckInterval, idleTimeout, receiveTimeout,
                             stateVar, uuid })
    = do

  generation <- readTVarIO generationVar
  debug uuid generation "connecting"

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
          atomically (writeTVar stateVar (Connected handle Healthy))

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
     ManagedBus
  -> Word64
  -> IO ()
monitorHealth
     managedBus@(ManagedBus { generationVar, healthCheckInterval, inFlightVar,
                              stateVar, uuid })
     generation = do

  debug uuid generation "healthy"
  monitorLoop

  where
    monitorLoop :: IO ()
    monitorLoop = do
      threadDelay healthCheckInterval

      atomicallyIO $
        whenGen generation generationVar $
          readTVar stateVar >>= \case
            Connected handle Healthy -> do
              incrTCounter inFlightVar

              pure $ do
                result :: Either Handle.HandleError (Either ByteString ()) <-
                  Handle.ping handle

                atomically (decrTCounter inFlightVar)

                case result of
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Healthy -> do
                            writeTVar stateVar Disconnecting
                            pure $ do
                              debug uuid generation ("health check failed: " ++ show err)
                              drainAndDisconnect inFlightVar handle
                              debug uuid generation "disconnected, reconnecting"
                              atomically $ do
                                modifyTVar' generationVar (+1)
                                writeTVar stateVar Connecting
                              connect managedBus

                          Disconnecting ->
                            pure (pure ())

                          Disconnected -> undefined
                          Connecting -> undefined
                          Connected _ Unhealthy -> undefined

                  Right (Left err) -> do
                    maybePingLoop err

                  Right (Right _) ->
                    monitorLoop

            Disconnecting ->
              pure (pure ())

            Disconnected -> undefined
            Connecting -> undefined
            Connected _ Unhealthy -> undefined

    maybePingLoop :: ByteString -> IO ()
    maybePingLoop err =
      atomicallyIO $
        whenGen generation generationVar $
          readTVar stateVar >>= \case
            Connected handle Healthy -> do
              writeTVar stateVar (Connected handle Unhealthy)

              pure $ do
                debug uuid generation $
                  "health check failed: " ++ show err ++ ", pinging until healthy"
                pingLoop 1

            Disconnected -> undefined
            Disconnecting -> undefined
            Connecting -> undefined
            Connected _ Unhealthy -> undefined

    pingLoop :: NominalDiffTime -> IO ()
    pingLoop seconds = do
      sleep seconds

      atomicallyIO $
        whenGen generation generationVar $
          readTVar stateVar >>= \case
            Connected handle Unhealthy -> do
              incrTCounter inFlightVar

              pure $ do
                result :: Either Handle.HandleError (Either ByteString ()) <-
                  Handle.ping handle

                atomically (decrTCounter inFlightVar)

                case result of
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Unhealthy -> do
                            writeTVar stateVar Disconnecting

                            pure $ do
                              debug uuid generation ("ping failed: " ++ show err)
                              drainAndDisconnect inFlightVar handle
                              debug uuid generation "disconnected, reconnecting"
                              atomically $ do
                                modifyTVar' generationVar (+1)
                                writeTVar stateVar Connecting
                              connect managedBus

                          Disconnecting ->
                            pure (pure ())

                          Disconnected -> undefined
                          Connecting -> undefined
                          Connected _ Healthy -> undefined

                  Right (Left err) -> do
                    debug uuid generation $
                      "ping failed: " ++ show err ++ ", retrying in " ++ show seconds
                    pingLoop (seconds * 1.5)

                  Right (Right _) ->
                    maybeMonitorLoop

            Disconnecting ->
              pure (pure ())

            Disconnected -> undefined
            Connecting -> undefined
            Connected _ Healthy -> undefined

    maybeMonitorLoop :: IO ()
    maybeMonitorLoop =
      atomicallyIO $
        whenGen generation generationVar $
          readTVar stateVar >>= \case
            Connected handle Unhealthy -> do
              writeTVar stateVar (Connected handle Healthy)
              pure $ do
                debug uuid generation "healthy"
                monitorLoop

            Disconnecting ->
              pure (pure ())

            Disconnected -> undefined
            Connecting -> undefined
            Connected _ Healthy -> undefined

monitorUsage ::
     ManagedBus
  -> Word64
  -> IO ()
monitorUsage
    (ManagedBus { generationVar, idleTimeout, inFlightVar, lastUsedRef,
                  stateVar, uuid })
    generation =

  loop

  where
    loop :: IO ()
    loop = do
      timer :: STM () <-
        registerOneShotEvent (idleTimeout `div` 2)

      atomicallyIO $
        (timer $> handleTimer)
        <|>
        (whenGen generation generationVar $
          readTVar stateVar >>= \case
            Connected _ Healthy -> retry
            Connected _ Unhealthy -> pure handleUnhealthy

            Disconnecting -> pure (pure ()) -- Should this be undefined?

            Disconnected -> undefined
            Connecting -> undefined)

      where
        handleTimer :: IO ()
        handleTimer = do
          now <- getMonotonicTimeNSec
          lastUsed <- readIORef lastUsedRef

          if now - lastUsed > fromIntegral (idleTimeout * 1000)
            then
              atomicallyIO $
                whenGen generation generationVar $
                  readTVar stateVar >>= \case
                    Connected handle _ -> do
                      writeTVar stateVar Disconnecting

                      pure $ do
                        debug uuid generation "idle timeout"
                        drainAndDisconnect inFlightVar handle
                        debug uuid generation "disconnected"
                        atomically $ do
                          modifyTVar' generationVar (+1)
                          writeTVar stateVar Disconnected

                    Disconnecting -> pure (pure ())

                    Connecting -> undefined
                    Disconnected -> undefined

            else
              loop

        -- Don't count an unhealthy socket against its idle timeout time,
        -- because requests cannot be made with it (so its lastUsed timestamp is
        -- static).
        handleUnhealthy :: IO ()
        handleUnhealthy =
          atomicallyIO $
            whenGen generation generationVar $
              readTVar stateVar >>= \case
                Connected _ Healthy -> pure loop
                Connected _ Unhealthy -> retry
                Disconnecting -> pure (pure ())

                Connecting -> undefined
                Disconnected -> undefined

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

whenGen :: Word64 -> TVar Word64 -> STM (IO ()) -> STM (IO ())
whenGen expected actualVar action = do
  actual <- readTVar actualVar
  if actual == expected
    then action
    else pure (pure ())

debug :: Int -> Word64 -> [Char] -> IO ()
debug uuid gen msg =
  Debug.debug ("handle " ++ show uuid ++ "." ++ show gen ++ ": " ++ msg)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))

atomicallyIO :: STM (IO a) -> IO a
atomicallyIO =
  join . atomically


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

listBuckets ::
     ManagedBus
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either ManagedBusError (Either ByteString r))
listBuckets bus request responseFold =
  withHandle bus $ \handle ->
    Handle.listBuckets handle request responseFold

listKeys ::
     ManagedBus
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either ManagedBusError (Either ByteString r))
listKeys bus request responseFold =
  withHandle bus $ \handle ->
    Handle.listKeys handle request responseFold

mapReduce ::
     ManagedBus
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either ManagedBusError (Either ByteString r))
mapReduce bus request responseFold =
  withHandle bus $ \handle ->
    Handle.mapReduce handle request responseFold

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

secondaryIndex ::
     ManagedBus
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either ManagedBusError (Either ByteString r))
secondaryIndex bus request responseFold =
  withHandle bus $ \handle ->
    Handle.secondaryIndex handle request responseFold

updateCrdt ::
     ManagedBus -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.DtUpdateResp))
updateCrdt bus request =
  withHandle bus $ \handle ->
    Handle.updateCrdt handle request
