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

-- TODO Rename ManagedBus

module RiakManagedBus
  ( ManagedBus
  , ManagedBusConfig(..)
  , EventHandlers(..)
  , ManagedBusError(..)
  , createManagedBus
    -- * API
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
import Libriak.Request    (Request)
import Libriak.Response   (DecodeError, Response)
import RiakError          (isAllNodesDownError, isDwValUnsatisfiedError,
                           isInsufficientVnodesError0,
                           isInsufficientVnodesError1, isOverloadError,
                           isPrValUnsatisfiedError, isPwValUnsatisfiedError,
                           isRValUnsatisfiedError, isTimeoutError,
                           isUnknownMessageCodeError, isWValUnsatisfiedError)

import qualified Libriak.Handle as Handle
import qualified RiakDebug      as Debug

import Control.Concurrent.STM
import Control.Exception.Safe (uninterruptibleMask, uninterruptibleMask_)
import Control.Foldl          (FoldM)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.Conc               (registerDelay)
import Socket.Stream.IPv4     (CloseException, ConnectException, Endpoint,
                               Interruptibility(..))

import qualified Data.Riak.Proto as Proto
import qualified TextShow


data ManagedBus
  = ManagedBus
  { uuid :: Int
  , endpoint :: Endpoint
  , healthCheckInterval :: Int
  , idleTimeout :: Int
  , requestTimeout :: Int
  , handlers :: EventHandlers

    -- See Note [State Machine]
  , generationVar :: TVar Word64
  , stateVar :: TVar State

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
  , requestTimeout :: Int -- Microseconds
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
  { onConnectAttempt :: Text -> IO ()
  , onConnectFailure :: Text -> ConnectException 'Uninterruptible -> IO ()
  , onConnectSuccess :: Text -> IO ()

  , onDisconnectAttempt :: Text -> IO ()
  , onDisconnectFailure :: Text -> CloseException -> IO ()
  , onDisconnectSuccess :: Text -> IO ()

    -- TODO uuid onSend/onReceive
  , onSend :: Request -> IO ()
  , onReceive :: Response -> IO ()

    -- TODO use onConnectionError
  , onConnectionError :: ConnectionError -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty mempty mempty mempty mempty
                         mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 e1 f1 g1 h1 i1 <>
    EventHandlers a2 b2 c2 d2 e2 f2 g2 h2 i2 =

    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
                  (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2)

instance Show EventHandlers where
  show _ =
    "<EventHandlers>"

makeId :: Int -> Word64 -> Text
makeId uuid generation =
  TextShow.toText (TextShow.showb uuid <> "." <> TextShow.showb generation)

-- | Create a managed bus.
--
-- /Throws/. This function will never throw an exception.
createManagedBus ::
     ManagedBusConfig
  -> IO ManagedBus
createManagedBus
    ManagedBusConfig { endpoint, handlers, healthCheckInterval, idleTimeout,
                       requestTimeout, uuid
                     } = do

  generationVar :: TVar Word64 <-
    newTVarIO 0

  stateVar :: TVar State <-
    newTVarIO Disconnected

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  pure ManagedBus
    { uuid = uuid
    , endpoint = endpoint
    , healthCheckInterval = healthCheckInterval
    , idleTimeout = idleTimeout
    , requestTimeout = requestTimeout
    , generationVar = generationVar
    , stateVar = stateVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    }

withHandle ::
     forall a.
     TVar Bool
  -> ManagedBus
  -> (TVar Bool -> Handle.Handle -> IO (Either Handle.HandleError a))
  -> IO (Either ManagedBusError a)
withHandle
    timeoutVar
    managedBus@(ManagedBus { generationVar, lastUsedRef, stateVar, uuid })
    callback =

  -- Mask because if we transition from Disconnected to Connected, we *must*
  -- successfully fork the connect thread
  uninterruptibleMask $ \unmask ->
    atomicallyIO $ do
      readTVar stateVar >>= \case
        Disconnected -> do
          writeTVar stateVar Connecting
          generation <- readTVar generationVar
          pure $ do
            void (forkIO (connect generation managedBus))
            unmask wait

        Connected handle Healthy -> do
          generation <- readTVar generationVar
          pure (unmask (withHealthyHandle generation handle))

        Connecting            -> pure (unmask wait)
        Connected _ Unhealthy -> pure (unmask wait)
        Disconnecting         -> pure (unmask wait)

  where
    withHealthyHandle ::
         Word64
      -> Handle.Handle
      -> IO (Either ManagedBusError a)
    withHealthyHandle generation handle = do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      callback timeoutVar handle >>= \case
        Left err -> do
          -- Mask because if we transition from Connected to Disconnecting,
          -- we *must* sucessfully fork the connect thread
          uninterruptibleMask_ $
            atomicallyIO $
              whenGen generation generationVar $
                readTVar stateVar >>= \case
                  Connected handle _ -> do
                    writeTVar stateVar Disconnecting

                    pure . void . forkIO $ do
                      debug uuid generation ("request failed: " ++ show err)
                      disconnect generation handle managedBus

                      atomically $ do
                        writeTVar generationVar (generation+1)
                        writeTVar stateVar Connecting

                      connect (generation+1) managedBus

                  Disconnecting ->
                    pure (pure ())

                  Disconnected -> undefined
                  Connecting -> undefined

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

        Right result ->
          pure (Right result)

    wait :: IO (Either ManagedBusError a)
    wait =
      atomicallyIO $
        (readTVar timeoutVar >>= \case
          False -> retry
          True  -> pure (pure (Left ManagedBusTimeoutError)))
        <|>
        (readTVar stateVar >>= \case
          Connected _ Healthy   -> pure (withHandle timeoutVar managedBus callback)
          Disconnected          -> pure (withHandle timeoutVar managedBus callback)

          Connecting            -> retry
          Connected _ Unhealthy -> retry
          Disconnecting         -> retry)

    fromHandleError :: Handle.HandleError -> ManagedBusError
    fromHandleError = \case
      Handle.HandleClosedError         -> ManagedBusPipelineError
      Handle.HandleConnectionError err -> ManagedBusConnectionError err
      Handle.HandleDecodeError     err -> ManagedBusDecodeError     err

-- TODO give up if bus gc'd
connect ::
     Word64 -- Invariant: this is what's in generationVar
  -> ManagedBus
  -> IO ()
connect
    generation
    managedBus@(ManagedBus { endpoint, handlers, healthCheckInterval,
                             idleTimeout, stateVar, uuid })
    = do

  debug uuid generation "connecting"
  connectLoop 1

  where
    connectLoop ::
         NominalDiffTime
      -> IO ()
    connectLoop seconds = do
      onConnectAttempt handlers ident
      Handle.connect endpoint handleHandlers >>= \case
        Left err -> do
          onConnectFailure handlers ident err
          sleep seconds
          connectLoop (seconds * 2)

        Right handle -> do
          onConnectSuccess handlers ident
          pingLoop handle seconds

    pingLoop :: Handle.Handle -> NominalDiffTime -> IO ()
    pingLoop handle seconds = do
      timeoutVar :: TVar Bool <-
        registerDelay pingTimeout

      Handle.ping timeoutVar handle >>= \case
        Left _ -> do
          disconnect generation handle managedBus
          sleep seconds
          connectLoop (seconds * 2)

        Right (Left _) -> do
          sleep seconds
          pingLoop handle (seconds * 2)

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
        -- , Handle.onError = mempty -- FIXME
        }

    ident :: Text
    ident =
      makeId uuid generation

monitorHealth ::
     ManagedBus
  -> Word64
  -> IO ()
monitorHealth
     managedBus@(ManagedBus { generationVar, healthCheckInterval, stateVar,
                              uuid })
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
            Connected handle Healthy ->
              pure $ do
                timeoutVar :: TVar Bool <-
                  registerDelay pingTimeout

                result :: Either Handle.HandleError (Either ByteString ()) <-
                  Handle.ping timeoutVar handle

                case result of
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Healthy -> do
                            writeTVar stateVar Disconnecting
                            pure $ do
                              debug uuid generation ("health check failed: " ++ show err)
                              disconnect generation handle managedBus
                              atomically $ do
                                writeTVar generationVar (generation+1)
                                writeTVar stateVar Connecting
                              connect (generation+1) managedBus

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
            Connected handle Unhealthy ->
              pure $ do
                timeoutVar :: TVar Bool <-
                  registerDelay pingTimeout

                result :: Either Handle.HandleError (Either ByteString ()) <-
                  Handle.ping timeoutVar handle

                case result of
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Unhealthy -> do
                            writeTVar stateVar Disconnecting

                            pure $ do
                              debug uuid generation ("ping failed: " ++ show err)
                              disconnect generation handle managedBus
                              atomically $ do
                                writeTVar generationVar (generation+1)
                                writeTVar stateVar Connecting
                              connect (generation+1) managedBus

                          Disconnecting ->
                            pure (pure ())

                          Disconnected -> undefined
                          Connecting -> undefined
                          Connected _ Healthy -> undefined

                  Right (Left err) -> do
                    debug uuid generation $
                      "ping failed: " ++ show err ++ ", retrying in " ++ show seconds
                    pingLoop (seconds * 2)

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
    managedBus@(ManagedBus { generationVar, idleTimeout, lastUsedRef, stateVar,
                             uuid })
    generation =

  loop

  where
    loop :: IO ()
    loop = do
      timeoutVar :: TVar Bool <-
        registerDelay (idleTimeout `div` 2)

      atomicallyIO $
        (readTVar timeoutVar >>= \case
          False -> retry
          True -> pure handleTimer)
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
                        disconnect generation handle managedBus
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

disconnect ::
     Word64
  -> Handle.Handle
  -> ManagedBus
  -> IO ()
disconnect generation handle ManagedBus { handlers, uuid } = do
  onDisconnectAttempt handlers ident
  Handle.disconnect handle >>= \case
    Left err -> onDisconnectFailure handlers ident err
    Right () -> onDisconnectSuccess handlers ident

  where
    ident :: Text
    ident =
      makeId uuid generation

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

pingTimeout :: Int
pingTimeout =
  5*1000*1000

--------------------------------------------------------------------------------
-- Libriak.Handle wrappers
--------------------------------------------------------------------------------

deleteIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either ManagedBusError (Either ByteString ()))
deleteIndex bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.deleteIndex timeoutVar handle request)

get ::
     ManagedBus
  -> Proto.RpbGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetResp))
get bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    getReqShouldRetry
    (translateTimeout <$>
      (withHandle timeoutVar bus $ \timeoutVar handle ->
        Handle.get timeoutVar handle request))

getBucket ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetBucketResp))
getBucket bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getBucket timeoutVar handle request)

getBucketType ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetBucketResp))
getBucketType bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getBucketType timeoutVar handle request)

getCrdt ::
     ManagedBus
  -> Proto.DtFetchReq
  -> IO (Either ManagedBusError (Either ByteString Proto.DtFetchResp))
getCrdt bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    getReqShouldRetry
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getCrdt timeoutVar handle request)

getIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getIndex timeoutVar handle request)

getSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getSchema timeoutVar handle request)

getServerInfo ::
     ManagedBus
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo bus@(ManagedBus { requestTimeout }) = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.getServerInfo timeoutVar handle)

listBuckets ::
     ManagedBus
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either ManagedBusError (Either ByteString r))
listBuckets bus@(ManagedBus { requestTimeout }) request responseFold = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.listBuckets timeoutVar handle request responseFold)

listKeys ::
     ManagedBus
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either ManagedBusError (Either ByteString r))
listKeys bus@(ManagedBus { requestTimeout }) request responseFold = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.listKeys timeoutVar handle request responseFold)

mapReduce ::
     ManagedBus
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either ManagedBusError (Either ByteString r))
mapReduce bus@(ManagedBus { requestTimeout }) request responseFold = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.mapReduce timeoutVar handle request responseFold)

ping ::
     ManagedBus
  -> IO (Either ManagedBusError (Either ByteString ()))
ping bus@(ManagedBus { requestTimeout }) = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.ping timeoutVar handle)

put ::
     ManagedBus
  -> Proto.RpbPutReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbPutResp))
put bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    putReqShouldRetry
    (translateTimeout <$>
      (withHandle timeoutVar bus $ \timeoutVar handle ->
        Handle.put timeoutVar handle request))

putIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either ManagedBusError (Either ByteString ()))
putIndex bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.putIndex timeoutVar handle request)

putSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either ManagedBusError (Either ByteString ()))
putSchema bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.putSchema timeoutVar handle request)

resetBucket ::
     ManagedBus
  -> Proto.RpbResetBucketReq
  -> IO (Either ManagedBusError (Either ByteString ()))
resetBucket bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.resetBucket timeoutVar handle request)

setBucket ::
     ManagedBus
  -> Proto.RpbSetBucketReq
  -> IO (Either ManagedBusError (Either ByteString ()))
setBucket bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  withHandle timeoutVar bus $ \timeoutVar handle ->
    Handle.setBucket timeoutVar handle request

setBucketType ::
     ManagedBus
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either ManagedBusError (Either ByteString ()))
setBucketType bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  withHandle timeoutVar bus $ \timeoutVar handle ->
    Handle.setBucketType timeoutVar handle request

search ::
     ManagedBus
  -> Proto.RpbSearchQueryReq
  -> IO (Either ManagedBusError (Either ByteString Proto.RpbSearchQueryResp))
search bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    isUnknownMessageCodeError
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.search timeoutVar handle request)

secondaryIndex ::
     ManagedBus
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either ManagedBusError (Either ByteString r))
secondaryIndex bus@(ManagedBus { requestTimeout }) request responseFold = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    (\err ->
      isInsufficientVnodesError0 err ||
      isUnknownMessageCodeError err)
    (withHandle timeoutVar bus $ \timeoutVar handle ->
      Handle.secondaryIndex timeoutVar handle request responseFold)

updateCrdt ::
     ManagedBus -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either ManagedBusError (Either ByteString Proto.DtUpdateResp))
updateCrdt bus@(ManagedBus { requestTimeout }) request = do
  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    putReqShouldRetry
    (translateTimeout <$>
      (withHandle timeoutVar bus $ \timeoutVar handle ->
        Handle.updateCrdt timeoutVar handle request))

-- TODO sleep for variable time depending on exact error?

getReqShouldRetry :: ByteString -> Bool
getReqShouldRetry err =
  isInsufficientVnodesError1 err ||
  isOverloadError err ||
  isPrValUnsatisfiedError err ||
  isRValUnsatisfiedError err ||
  isUnknownMessageCodeError err

putReqShouldRetry :: ByteString -> Bool
putReqShouldRetry err =
  isAllNodesDownError err ||
  isDwValUnsatisfiedError err ||
  isOverloadError err ||
  isPwValUnsatisfiedError err ||
  isUnknownMessageCodeError err ||
  isWValUnsatisfiedError err

retrying ::
     forall r.
     TVar Bool
  -> (ByteString -> Bool)
  -> IO (Either ManagedBusError (Either ByteString r))
  -> IO (Either ManagedBusError (Either ByteString r))
retrying timeoutVar shouldRetry action =
  retrying_ timeoutVar shouldRetry action (1*1000*1000)

retrying_ ::
     forall r.
     TVar Bool
  -> (ByteString -> Bool)
  -> IO (Either ManagedBusError (Either ByteString r))
  -> Int
  -> IO (Either ManagedBusError (Either ByteString r))
retrying_ timeoutVar shouldRetry action =
  loop

  where
    loop :: Int -> IO (Either ManagedBusError (Either ByteString r))
    loop sleepMicros =
      action >>= \case
        Right (Left err) | shouldRetry err -> do
          sleepVar :: TVar Bool <-
            registerDelay sleepMicros

          atomicallyIO $
            (readTVar sleepVar >>= \case
              False -> retry
              True -> pure (loop (sleepMicros * 2)))
            <|>
            (readTVar timeoutVar >>= \case
              False -> retry
              True -> pure (pure (Left ManagedBusTimeoutError)))

        result ->
          pure result

-- Translate "timeout" error to a managed bus timeout.
translateTimeout ::
     Either ManagedBusError (Either ByteString r)
  -> Either ManagedBusError (Either ByteString r)
translateTimeout = \case
  Right (Left err) | isTimeoutError err ->
    Left ManagedBusTimeoutError

  result ->
    result


--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

-- Note [State Machine]
--
-- Together, stateVar and generationVar determine the state of the socket.
-- Generation is monotonically increasing.
--
-- * {Disconnected N}
--   * {Connecting N}          if request arrives
--
-- * {Connecting N}
--   * {Connecting N}          if connect fails or initial ping fails
--   * {Connected-Healthy N}   if initial ping succeeds
--
-- * {Connected-Healthy N}
--   * {Connected-Unhealthy N} if background ping fails (Riak error)
--   * {Disconnecting N}       if request fails (handle error) or idle timeout
--
-- * {Connected-Unhealthy N}
--   * {Connected-Unhealthy N} if background ping fails (Riak error)
--   * {Connected-Healthy N}   if background ping succeeds
--   * {Disconnecting N}       if request fails (handle error)
--
-- * {Disconnecting N}
--   * {Connecting N+1}        if disconnected due to handle error
--   * {Disconnected N+1}      if disconnected due to idle timeout
--
-- This explains why there are so many 'undefined' in the code: many state
-- transitions are simply impossible.
--
-- Example run:
--
-- {Disconnected 0}
--   => Request arrives
-- {Connecting 0}
--   => Connect fails (sleep 1s)
--   => Connect succeeds
--   => Initial ping fails due to handle error, disconnect (sleep 1s)
--   => Connect succeeds
--   => Initial ping fails due to Riak error (sleep 1s)
--   => Initial ping fails due to Riak error (sleep 2s)
--   => Initial ping succeeds
-- {Connected-Healthy 0}
--   => Background ping fails due to Riak error
-- {Connected-Unhealthy 0}
--   => Background ping fails due to Riak error (sleep 1s)
--   => Background ping fails due to Riak error (sleep 2s)
--   => Background ping succeeds
-- {Connected-Healthy 0}
--   => Background ping fails due to handle error
-- {Disconnecting 0}
--   => Disconnected
-- {Connecting 1}
--   => Connect succeeds
--   => Initial ping succeeds
-- {Connected-Healthy 1}
--   => Idle timeout
-- {Disconnecting 1}
--   => Disconnected
-- {Disconnected 2}
