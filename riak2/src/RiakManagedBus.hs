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
--   If the connect times out per 'connectTimeout' setting,
--     ==> [Disconnected]
--
--   If the connect fails for any other reason,
--     ==> [Connecting]
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
--
-- TODO if request times out before it's even sent we don't have to close the
-- socket
--
-- TODO replace undefined with more informative "state machine error"
--
-- TODO why is timeoutVar threaded?

module RiakManagedBus
  ( ManagedBus
  , BusId
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
import Libriak.Response   (DecodeError)
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
import Control.Foldl          (FoldM(..))
import Control.Lens           ((^.))
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.Conc               (registerDelay)
import Socket.Stream.IPv4     (CloseException, ConnectException(..), Endpoint,
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
  , connectTimeout :: Int
  , handlers :: EventHandlers

    -- See Note [State Machine]
  , generationVar :: TVar Word64
  , stateVar :: TVar State

  , lastUsedRef :: IORef Word64
    -- ^ The last time the bus was used.
  }

type BusId
  = Text

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
  -- TODO reorder these
  , idleTimeout :: Int -- Microseconds
  , requestTimeout :: Int -- Microseconds
  , connectTimeout :: Int -- Microseconds
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
  { onConnectAttempt :: BusId -> IO ()
  , onConnectFailure :: BusId -> ConnectException 'Interruptible -> IO ()
  , onConnectSuccess :: BusId -> IO ()

    -- TODO add disconnect reason argument
  , onDisconnectAttempt :: BusId -> IO ()
  , onDisconnectFailure :: BusId -> CloseException -> IO ()
  , onDisconnectSuccess :: BusId -> IO ()

  , onSend :: forall a. (Proto.Message a, Show a) => Text -> a -> IO ()
  , onReceive :: forall a. (Proto.Message a, Show a) => Text -> a -> IO ()

    -- TODO use onConnectionError
  , onConnectionError :: ConnectionError -> IO ()

  , onIdleTimeout :: BusId -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty mempty mempty mempty mempty
                         mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 <>
    EventHandlers a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 =

    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
                  (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2)

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
    ManagedBusConfig { connectTimeout, endpoint, handlers, healthCheckInterval,
                       idleTimeout, requestTimeout, uuid
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
    , connectTimeout = connectTimeout
    , generationVar = generationVar
    , stateVar = stateVar
    , lastUsedRef = lastUsedRef
    , handlers = handlers
    }

withHandle ::
     forall a.
     TVar Bool
  -> ManagedBus
  -> ( BusId
    -> TVar Bool
    -> Handle.Handle
    -> IO (Either Handle.HandleError (Either Proto.RpbErrorResp a)))
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
withHandle
    timeoutVar
    managedBus@(ManagedBus { generationVar, lastUsedRef, stateVar, uuid })
    callback =

  mainLoop

  where
    mainLoop :: IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
    mainLoop =
      -- Mask because if we transition from Disconnected to Connected, we *must*
      -- successfully fork the connect thread
      uninterruptibleMask $ \unmask ->
        atomicallyIO $ do
          generation :: Word64 <-
            readTVar generationVar

          readTVar stateVar >>= \case
            Disconnected -> do
              writeTVar stateVar Connecting
              pure $ do
                void (forkIO (connect generation managedBus))
                unmask (connectingWait generation)

            Connected handle Healthy ->
              pure (unmask (withHealthyHandle generation handle))

            Connecting ->
              pure (unmask (connectingWait generation))

            Connected _ Unhealthy ->
              pure (unmask (unhealthyWait generation))

            Disconnecting ->
              pure (unmask (disconnectingWait generation))

    withHealthyHandle ::
         Word64
      -> Handle.Handle
      -> IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
    withHealthyHandle generation handle = do
      writeIORef lastUsedRef =<<
        getMonotonicTimeNSec

      callback (makeId uuid generation) timeoutVar handle >>= \case
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

                      disconnect
                        Handle.hardDisconnect
                        generation
                        handle
                        managedBus

                      atomically $ do
                        writeTVar generationVar (generation+1)
                        writeTVar stateVar Connecting

                      connect (generation+1) managedBus

                  Disconnecting ->
                    pure (pure ())

                  Disconnected -> undefined
                  Connecting -> undefined

          -- Wait for us to transition off of a healthy status before
          -- returning, so that the caller may immediately retry and not hit
          -- this same code path.
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

    connectingWait ::
         Word64
      -> IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
    connectingWait generation =
      atomicallyIO $
        blockUntilRequestTimeout
        <|>
        (retryIfNewGeneration generation $
          readTVar stateVar >>= \case
            Connecting ->
              retry

            Connected _ Healthy ->
              pure mainLoop

            Connected _ Unhealthy ->
              pure (unhealthyWait generation)

            Disconnecting ->
              pure (disconnectingWait generation)

            Disconnected ->
              pure (pure (Left ManagedBusTimeoutError)))

    unhealthyWait ::
         Word64
      -> IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
    unhealthyWait generation =
      atomicallyIO $
        blockUntilRequestTimeout
        <|>
        (retryIfNewGeneration generation $
          readTVar stateVar >>= \case
            Connected _ Unhealthy ->
              retry

            Connected _ Healthy ->
              pure mainLoop

            Disconnecting ->
              pure (disconnectingWait generation)

            Connecting -> undefined
            Disconnected -> undefined)

    disconnectingWait ::
         Word64
      -> IO (Either ManagedBusError (Either Proto.RpbErrorResp a))
    disconnectingWait generation =
      atomicallyIO $
        blockUntilRequestTimeout
        <|>
        -- 'Disconnecting N' can either transition to 'Connecting N+1' or
        -- 'Disconnected N+1'. We just need to wait until either happens.
        (readTVar generationVar >>= \generation' ->
          if generation == generation'
            then retry
            else pure mainLoop)

    blockUntilRequestTimeout ::
         STM (IO (Either ManagedBusError (Either Proto.RpbErrorResp a)))
    blockUntilRequestTimeout =
      readTVar timeoutVar >>= \case
        False -> retry
        True -> pure (pure (Left ManagedBusTimeoutError))

    retryIfNewGeneration ::
         Word64
      -> STM (IO (Either ManagedBusError (Either Proto.RpbErrorResp a)))
      -> STM (IO (Either ManagedBusError (Either Proto.RpbErrorResp a)))
    retryIfNewGeneration expectedGen action = do
      actualGen <- readTVar generationVar
      if actualGen == expectedGen
        then action
        else pure mainLoop

    fromHandleError :: Handle.HandleError -> ManagedBusError
    fromHandleError = \case
      Handle.HandleClosedError         -> ManagedBusPipelineError
      Handle.HandleConnectionError err -> ManagedBusConnectionError err
      Handle.HandleDecodeError     err -> ManagedBusDecodeError     err

-- TODO give up if bus gc'd
-- TODO does it matter that async exceptions are masked here?
connect ::
     Word64 -- Invariant: this is what's in generationVar
  -> ManagedBus
  -> IO ()
connect
    generation
    managedBus@(ManagedBus { endpoint, connectTimeout, handlers,
                             healthCheckInterval, idleTimeout, requestTimeout,
                             stateVar, uuid })
    = do

  timeoutVar <- registerDelay connectTimeout
  connectLoop timeoutVar 1

  where
    connectLoop ::
         TVar Bool
      -> NominalDiffTime
      -> IO ()
    connectLoop timeoutVar seconds = do
      onConnectAttempt handlers ident

      Handle.connect timeoutVar endpoint >>= \case
        Left err -> do
          onConnectFailure handlers ident err

          case err of
            ConnectInterrupted ->
              atomically (writeTVar stateVar Disconnected)

            _ -> do
              sleep seconds
              connectLoop timeoutVar (seconds * 2)

        Right handle -> do
          onConnectSuccess handlers ident
          pingLoop handle seconds

    pingLoop :: Handle.Handle -> NominalDiffTime -> IO ()
    pingLoop handle seconds = do
      timeoutVar :: TVar Bool <-
        registerDelay requestTimeout

      Handle.ping timeoutVar handle >>= \case
        Left _ -> do
          disconnect Handle.hardDisconnect generation handle managedBus
          sleep seconds
          connectLoop timeoutVar (seconds * 2)

        Right (Left _) -> do
          sleep seconds
          pingLoop handle (seconds * 2)

        Right (Right _) -> do
          atomically (writeTVar stateVar (Connected handle Healthy))

          when (healthCheckInterval > 0)
            (void (forkIO (monitorHealth managedBus generation)))

          when (idleTimeout > 0)
            (void (forkIO (monitorUsage managedBus generation)))

    ident :: Text
    ident =
      makeId uuid generation

monitorHealth ::
     ManagedBus
  -> Word64
  -> IO ()
monitorHealth
     managedBus@(ManagedBus { generationVar, healthCheckInterval,
                              requestTimeout, stateVar, uuid })
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
                  registerDelay requestTimeout

                Handle.ping timeoutVar handle >>= \case
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Healthy -> do
                            writeTVar stateVar Disconnecting

                            pure $ do
                              debug uuid generation ("health check failed: " ++ show err)
                              disconnect
                                Handle.hardDisconnect
                                generation
                                handle
                                managedBus

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

    maybePingLoop :: Proto.RpbErrorResp -> IO ()
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

            Disconnecting ->
              pure (pure ())

            Disconnected -> undefined
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
                  registerDelay requestTimeout

                -- TODO share code with monitorLoop
                Handle.ping timeoutVar handle >>= \case
                  Left err ->
                    atomicallyIO $
                      whenGen generation generationVar $
                        readTVar stateVar >>= \case
                          Connected handle Unhealthy -> do
                            writeTVar stateVar Disconnecting

                            pure $ do
                              debug uuid generation ("ping failed: " ++ show err)

                              disconnect
                                Handle.hardDisconnect
                                generation
                                handle
                                managedBus

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
    managedBus@(ManagedBus { generationVar, handlers, idleTimeout, lastUsedRef,
                             stateVar, uuid })
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
            Connected _ Healthy ->
              retry

            Connected _ Unhealthy ->
              pure handleUnhealthy

            Disconnecting ->
              pure (pure ())

            Disconnected -> undefined
            Connecting -> undefined)

      where
        handleTimer :: IO ()
        handleTimer = do
          now :: Word64 <-
            getMonotonicTimeNSec

          lastUsed :: Word64 <-
            readIORef lastUsedRef

          if now - lastUsed > fromIntegral (idleTimeout * 1000)
            then
              atomicallyIO $
                whenGen generation generationVar $
                  readTVar stateVar >>= \case
                    Connected handle _ -> do
                      writeTVar stateVar Disconnecting

                      pure $ do
                        onIdleTimeout handlers ident

                        disconnect
                          Handle.softDisconnect
                          generation
                          handle
                          managedBus

                        atomically $ do
                          writeTVar generationVar (generation+1)
                          writeTVar stateVar Disconnected

                    Disconnecting ->
                      pure (pure ())

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
                Connected _ Healthy ->
                  pure loop

                Connected _ Unhealthy ->
                  retry

                Disconnecting ->
                  pure (pure ())

                Connecting -> undefined
                Disconnected -> undefined

    ident :: Text
    ident =
      makeId uuid generation

disconnect ::
     (Handle.Handle -> IO (Either CloseException ()))
  -> Word64
  -> Handle.Handle
  -> ManagedBus
  -> IO ()
disconnect doDisconnect generation handle ManagedBus { handlers, uuid } = do
  onDisconnectAttempt handlers ident

  doDisconnect handle >>= \case
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


--------------------------------------------------------------------------------
-- Libriak.Handle wrappers
--------------------------------------------------------------------------------

exchange ::
     ( Proto.Message a
     , Proto.Message b
     , Show a
     , Show b
     )
  => ManagedBus
  -> a
  -> (ByteString -> Bool)
  -> ( TVar Bool
    -> Handle.Handle
    -> a
    -> IO (Either Handle.HandleError (Either Proto.RpbErrorResp b)))
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp b))
exchange
    bus@(ManagedBus { handlers, requestTimeout })
    request
    shouldRetry
    performRequest = do

  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    shouldRetry
    (withHandle timeoutVar bus $ \ident timeoutVar handle -> do
      onSend handlers ident request

      performRequest timeoutVar handle request >>= \case
        Left err ->
          pure (Left err)

        Right response -> do
          either (onReceive handlers ident) (onReceive handlers ident) response
          pure (Right response))

stream ::
     forall a b r.
     ( Proto.Message a
     , Proto.Message b
     , Show a
     , Show b
     )
  => ManagedBus
  -> a
  -> FoldM IO b r
  -> (ByteString -> Bool)
  -> ( TVar Bool
    -> Handle.Handle
    -> a
    -> FoldM IO b r
    -> IO (Either Handle.HandleError (Either Proto.RpbErrorResp r)))
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
stream
    bus@(ManagedBus { handlers, requestTimeout })
    request
    (FoldM step initial extract)
    shouldRetry
    performRequest = do

  timeoutVar :: TVar Bool <-
    registerDelay requestTimeout

  retrying
    timeoutVar
    shouldRetry
    (withHandle timeoutVar bus $ \ident timeoutVar handle -> do
      onSend handlers ident request
      performRequest timeoutVar handle request (responseFold ident))

  where
    responseFold :: BusId -> FoldM IO b r
    responseFold ident =
      FoldM
        (\acc response -> do
          onReceive handlers ident response
          step acc response)
        initial
        extract

deleteIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbDelResp))
deleteIndex bus request =
  exchange bus request isUnknownMessageCodeError Handle.deleteIndex

get ::
     ManagedBus
  -> Proto.RpbGetReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbGetResp))
get bus request =
  translateTimeout <$>
    exchange bus request getReqShouldRetry Handle.get

getBucket ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbGetBucketResp))
getBucket bus request = do
  exchange bus request isUnknownMessageCodeError Handle.getBucket

getBucketType ::
     ManagedBus -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbGetBucketResp))
getBucketType bus request =
  exchange bus request isUnknownMessageCodeError Handle.getBucketType

getCrdt ::
     ManagedBus
  -> Proto.DtFetchReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.DtFetchResp))
getCrdt bus request =
  exchange bus request getReqShouldRetry Handle.getCrdt

getIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbYokozunaIndexGetResp))
getIndex bus request =
  exchange bus request isUnknownMessageCodeError Handle.getIndex

getSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbYokozunaSchemaGetResp))
getSchema bus request =
  exchange bus request isUnknownMessageCodeError Handle.getSchema

getServerInfo ::
     ManagedBus
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbGetServerInfoResp))
getServerInfo bus =
  exchange
    bus
    (Proto.defMessage :: Proto.RpbGetServerInfoReq)
    isUnknownMessageCodeError
    (\timeoutVar handle _ -> Handle.getServerInfo timeoutVar handle)

listBuckets ::
     ManagedBus
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
listBuckets bus request responseFold =
  stream bus request responseFold isUnknownMessageCodeError Handle.listBuckets

listKeys ::
     ManagedBus
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
listKeys bus request responseFold =
  stream bus request responseFold isUnknownMessageCodeError Handle.listKeys

mapReduce ::
     ManagedBus
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
mapReduce bus request responseFold =
  stream bus request responseFold isUnknownMessageCodeError Handle.mapReduce

ping ::
     ManagedBus
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbPingResp))
ping bus =
  exchange
    bus
    (Proto.defMessage :: Proto.RpbPingReq)
    isUnknownMessageCodeError
    (\timeoutVar handle _ -> Handle.ping timeoutVar handle)

put ::
     ManagedBus
  -> Proto.RpbPutReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbPutResp))
put bus request =
  translateTimeout <$>
    exchange bus request putReqShouldRetry Handle.put

putIndex ::
     ManagedBus
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbPutResp))
putIndex bus request =
  exchange bus request isUnknownMessageCodeError Handle.putIndex

putSchema ::
     ManagedBus
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbPutResp))
putSchema bus request =
  exchange bus request isUnknownMessageCodeError Handle.putSchema

resetBucket ::
     ManagedBus
  -> Proto.RpbResetBucketReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbResetBucketResp))
resetBucket bus request =
  exchange bus request isUnknownMessageCodeError Handle.resetBucket

setBucket ::
     ManagedBus
  -> Proto.RpbSetBucketReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbSetBucketResp))
setBucket bus request =
  -- TODO can we see UnknownMessageCode on setBucket(Type)?
  exchange bus request (const False) Handle.setBucket

setBucketType ::
     ManagedBus
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbSetBucketResp))
setBucketType bus request =
  exchange bus request (const False) Handle.setBucketType

search ::
     ManagedBus
  -> Proto.RpbSearchQueryReq
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.RpbSearchQueryResp))
search bus request =
  exchange bus request isUnknownMessageCodeError Handle.search

secondaryIndex ::
     ManagedBus
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
secondaryIndex bus request responseFold =
  stream bus request responseFold shouldRetry Handle.secondaryIndex

  where
    shouldRetry :: ByteString -> Bool
    shouldRetry err =
      isInsufficientVnodesError0 err ||
      isUnknownMessageCodeError err

updateCrdt ::
     ManagedBus -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp Proto.DtUpdateResp))
updateCrdt bus request =
  translateTimeout <$>
    exchange bus request putReqShouldRetry Handle.updateCrdt

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
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
retrying timeoutVar shouldRetry action =
  retrying_ timeoutVar shouldRetry action (1*1000*1000)

retrying_ ::
     forall r.
     TVar Bool
  -> (ByteString -> Bool)
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
  -> Int
  -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
retrying_ timeoutVar shouldRetry action =
  loop

  where
    loop :: Int -> IO (Either ManagedBusError (Either Proto.RpbErrorResp r))
    loop sleepMicros = do
      action >>= \case
        Right (Left err) | shouldRetry (err ^. Proto.errmsg) -> do
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
     Either ManagedBusError (Either Proto.RpbErrorResp r)
  -> Either ManagedBusError (Either Proto.RpbErrorResp r)
translateTimeout = \case
  Right (Left err) | isTimeoutError (err ^. Proto.errmsg) ->
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
--   * {Connecting N}          if connect fails (non-timeout) or initial ping
--                             fails
--   * {Connected-Healthy N}   if initial ping succeeds
--   * {Disconnected N}        if connect times out per 'connectTimeout'
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
