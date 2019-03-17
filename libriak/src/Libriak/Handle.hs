{-# LANGUAGE CPP #-}

module Libriak.Handle
  ( Handle
  , EventHandlers(..)
  , HandleError(..)
  , connect
  , disconnect
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

import Libriak.Connection (Connection, ConnectionError(..))
import Libriak.Request
import Libriak.Response

import qualified Libriak.Connection as Connection

import Control.Applicative                ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Foldl                      (FoldM(..))
import Control.Lens                       ((^.))
import Control.Monad                      (join)
import Data.ByteString                    (ByteString)
import Data.Function                      (on)
import Data.Kind                          (Type)
import Data.NF                            (NF, getNF, makeNF)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Socket.Stream.IPv4                 (CloseException, ConnectException(..),
                                           Endpoint(..), Interruptibility(..))

import qualified Control.Foldl   as Foldl
import qualified Data.Riak.Proto as Proto


data Handle
  = Handle
  { connection :: Connection
  , stateVar :: TVar State
  , sendAsync :: Async ()
  , receiveAsync :: Async ()
  , queue :: TBQueue Item
  , handlers :: EventHandlers
  }

instance Eq Handle where
  (==) =
    (==) `on` stateVar

data State
  = Healthy
  | Unhealthy
  | Disconnected

data Item :: Type where
  Exchange ::
       TVar Bool
    -> NF EncodedRequest
    -> TMVar (Either HandleError EncodedResponse)
    -> Item

  Stream ::
       HasLens' response "done" Bool
    => TVar Bool
    -> NF EncodedRequest
    -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
    -> TQueue (Either HandleError (Either Proto.RpbErrorResp response))
    -> Item

data SentItem :: Type where
  Exchange' ::
       TVar Bool
    -> TMVar (Either HandleError EncodedResponse)
    -> SentItem

  Stream' ::
       HasLens' response "done" Bool
    => TVar Bool
    -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
    -> TQueue (Either HandleError (Either Proto.RpbErrorResp response))
    -> SentItem

data HandleError :: Type where
  -- | The handle is permanently closed.
  HandleClosedError :: HandleError
  -- | A connection error occurred during a send or receive.
  HandleConnectionError :: ConnectionError -> HandleError
  -- | A protobuf decode error occurred.
  HandleDecodeError :: DecodeError -> HandleError
  deriving stock (Show)

data EventHandlers
  = EventHandlers
  { onSend :: Request -> IO ()
    -- ^ Called just prior to enqueuing a request. Must not throw an exception.
  , onReceive :: Response -> IO ()
  --   -- ^ Called just after receiving a response.
  -- -- , onError :: HandleError -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty -- mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2) -- (c1 <> c2)

-- | Acquire a handle.
--
-- This function must be called with asynchronous exceptions masked, and the
-- caller must eventually call 'disconnect'.
--
-- /Throws/: This function will never throw an exception.
connect ::
     TVar Bool
  -> Endpoint
  -> EventHandlers
  -> IO (Either (ConnectException 'Interruptible) Handle)
connect timeoutVar endpoint handlers =
  Connection.connect timeoutVar endpoint >>= \case
    Left err ->
      pure (Left err)

    Right connection -> do
      stateVar :: TVar State <-
        newTVarIO Healthy

      streamingVar :: TVar Bool <-
        newTVarIO False

      itemQueue :: TBQueue Item <-
        newTBQueueIO (2^(14::Int))

      sentItemQueue :: TQueue SentItem <-
        newTQueueIO

      sendAsync :: Async () <-
        asyncWithUnmask $ \unmask ->
          unmask
            (sendThread
              stateVar
              streamingVar
              itemQueue
              sentItemQueue
              connection)

      receiveAsync :: Async () <-
        asyncWithUnmask $ \unmask ->
          unmask
            (receiveThread
              stateVar
              streamingVar
              sentItemQueue
              connection)

      pure (Right Handle
        { connection = connection
        , stateVar = stateVar
        , sendAsync = sendAsync
        , receiveAsync = receiveAsync
        , queue = itemQueue
        , handlers = handlers
        })

sendThread ::
     TVar State
  -> TVar Bool
  -> TBQueue Item
  -> TQueue SentItem
  -> Connection
  -> IO ()
sendThread stateVar streamingVar itemQueue sentItemQueue connection =
  loop

  where
    loop :: IO ()
    loop =
      join . atomically $
        processItem
        <|>
        (pure () <$ returnIfNotHealthy stateVar)

    processItem :: STM (IO ())
    processItem =
      sendItem <$>
        readTBQueue itemQueue

    sendItem :: Item -> IO ()
    sendItem = \case
      Exchange timeoutVar request responseVar ->
        Connection.send connection (getNF request) >>= \case
          Left err ->
            atomically $ do
              putTMVar
                responseVar
                (Left (HandleConnectionError err))

              healthyToUnhealthy stateVar

          Right () -> do
            atomically
              (writeTQueue sentItemQueue (Exchange' timeoutVar responseVar))

            loop

      Stream timeoutVar request decodeResponse responseQueue ->
        Connection.send connection (getNF request) >>= \case
          Left err ->
            atomically $ do
              writeTQueue
                responseQueue
                (Left (HandleConnectionError err))

              healthyToUnhealthy stateVar

          Right () -> do
            atomically $ do
              writeTVar streamingVar True

              writeTQueue
                sentItemQueue
                (Stream'
                  timeoutVar
                  decodeResponse
                  responseQueue)

            join . atomically $
              (readTVar streamingVar >>= \case
                True -> retry
                False -> pure loop)
              <|>
              (pure () <$ returnIfNotHealthy stateVar)

receiveThread ::
     TVar State
  -> TVar Bool
  -> TQueue SentItem
  -> Connection
  -> IO ()
receiveThread stateVar streamingVar sentItemQueue connection =
  loop

  where
    loop :: IO ()
    loop =
      join . atomically $
        (handleItem <$> readTQueue sentItemQueue)
        <|>
        (pure () <$ returnIfNotHealthy stateVar)

    handleItem :: SentItem -> IO ()
    handleItem = \case
      Exchange' timeoutVar responseVar ->
        Connection.receive timeoutVar connection >>= \case
          Left err ->
            atomically $ do
              putTMVar responseVar (Left (HandleConnectionError err))
              writeTVar stateVar Disconnected

          Right response -> do
            atomically (putTMVar responseVar (Right response))
            loop

      Stream' timeoutVar decodeResponse responseQueue -> do
        streamResponse timeoutVar decodeResponse responseQueue

    streamResponse ::
         forall response.
         HasLens' response "done" Bool
      => TVar Bool
      -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
      -> TQueue (Either HandleError (Either Proto.RpbErrorResp response))
      -> IO ()
    streamResponse timeoutVar decodeResponse responseQueue =
      receiveLoop

      where
        receiveLoop :: IO ()
        receiveLoop =
          Connection.receive timeoutVar connection >>= \case
            Left err ->
              atomically $ do
                writeTQueue responseQueue (Left (HandleConnectionError err))
                writeTVar stateVar Disconnected

            Right response ->
              case decodeResponse response of
                Left err ->
                  -- TODO Hrm, thread that called 'stream' might see
                  -- Disconnected before its own decode error... ;(
                  atomically $ do
                    writeTQueue responseQueue (Left (HandleDecodeError err))
                    writeTVar stateVar Disconnected

                Right (Left response) -> do
                  atomically $ do
                    writeTQueue responseQueue (Right (Left response))
                    writeTVar streamingVar False
                  loop

                Right (Right response) -> do
                  atomically (writeTQueue responseQueue (Right (Right response)))

                  if response ^. Proto.done
                    then do
                      atomically (writeTVar streamingVar False)
                      loop
                    else
                      receiveLoop

healthyToUnhealthy :: TVar State -> STM ()
healthyToUnhealthy stateVar =
  readTVar stateVar >>= \case
    Healthy -> writeTVar stateVar Unhealthy
    Unhealthy -> pure ()
    Disconnected -> pure ()

returnIfNotHealthy :: TVar State -> STM ()
returnIfNotHealthy stateVar =
  readTVar stateVar >>= \case
    Healthy -> retry
    Unhealthy -> pure ()
    Disconnected -> pure ()

-- | Disconnect a handle if it's not already disconnected.
--
-- /Throws/: This function will never throw an exception.
disconnect ::
     Handle
  -> IO (Either CloseException ())
disconnect Handle { connection, sendAsync, receiveAsync, stateVar } =
  join . atomically $
    readTVar stateVar >>= \case
      Healthy -> doDisconnect
      Unhealthy -> doDisconnect
      Disconnected -> pure (pure (Right ()))

  where
    doDisconnect :: STM (IO (Either CloseException ()))
    doDisconnect = do
      writeTVar stateVar Disconnected
      pure $ do
        wait sendAsync
        wait receiveAsync
        Connection.disconnect connection

-- | Send a request and receive the response (a single message).
exchange ::
     forall response.
     TVar Bool
  -> Handle
  -> EncodedRequest
  -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
  -> IO (Either HandleError (Either Proto.RpbErrorResp response))
exchange
    timeoutVar
    Handle { queue, stateVar }
    request
    fromResponse =

  readTVarIO stateVar >>= \case
    Healthy -> do
      responseVar :: TMVar (Either HandleError EncodedResponse) <-
        newEmptyTMVarIO

      atomically
        (writeTBQueue
          queue
          (Exchange timeoutVar (makeNF request) responseVar))

      join . atomically $
        (do
          result :: Either HandleError EncodedResponse <-
            readTMVar responseVar

          pure $
            case result of
              Left err ->
                pure (Left err)

              Right encodedResponse ->
                case fromResponse encodedResponse of
                  Left err -> do
                    atomically (healthyToUnhealthy stateVar)
                    pure (Left (HandleDecodeError err))

                  Right (Left response) ->
                    pure (Right (Left response))

                  Right (Right response) ->
                    pure (Right (Right response)))
        <|>
        (pure (Left HandleClosedError) <$ returnIfNotHealthy stateVar)

    Unhealthy    -> pure (Left HandleClosedError)
    Disconnected -> pure (Left HandleClosedError)


-- | Send a request and stream the response (one or more messages).
stream ::
     forall r response.
     HasLens' response "done" Bool
  => TVar Bool
  -> Handle -- ^
  -> EncodedRequest -- ^
  -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
  -> FoldM IO response r
  -> IO (Either HandleError (Either Proto.RpbErrorResp r))
stream
    timeoutVar
    (Handle { queue, stateVar })
    request
    decodeResponse
    (FoldM step (initial :: IO x) extract) =

  readTVarIO stateVar >>= \case
    Healthy -> do
      responseQueue :: TQueue (Either HandleError (Either Proto.RpbErrorResp response)) <-
        newTQueueIO

      atomically
        (writeTBQueue
          queue
          (Stream
            timeoutVar
            (makeNF request)
            decodeResponse
            responseQueue))

      processResponse responseQueue

    Unhealthy    -> pure (Left HandleClosedError)
    Disconnected -> pure (Left HandleClosedError)

  where
    processResponse ::
         TQueue (Either HandleError (Either Proto.RpbErrorResp response))
      -> IO (Either HandleError (Either Proto.RpbErrorResp r))
    processResponse responseQueue =
      initial >>= loop

      where
        loop :: x -> IO (Either HandleError (Either Proto.RpbErrorResp r))
        loop acc =
          join . atomically $
            (do
              response :: Either HandleError (Either Proto.RpbErrorResp response) <-
                readTQueue responseQueue

              pure $
                case response of
                  Left err ->
                    pure (Left err)

                  Right (Left response) ->
                    pure (Right (Left response))

                  Right (Right response) -> do
                    acc' :: x <-
                      step acc response

                    if response ^. Proto.done
                      then
                        Right . Right <$>
                          extract acc'

                      else
                        loop acc')
            <|>
            (pure (Left HandleClosedError) <$ returnIfNotHealthy stateVar)

--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

doExchange ::
     (Response -> IO ())
  -> (a -> Response)
  -> IO (Either HandleError (Either Proto.RpbErrorResp a))
  -> IO (Either HandleError (Either ByteString a))
doExchange onReceive toResponse action =
  action >>= \case
    Left err ->
      pure (Left err)

    Right (Left response) -> do
      onReceive (RespRpbError response)
      pure (Right (Left (response ^. Proto.errmsg)))

    Right (Right response) -> do
      onReceive (toResponse response)
      pure (Right (Right response))

delete ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbDelReq -- ^
  -> IO (Either HandleError (Either ByteString ()))
delete timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbDel request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbDel
      (exchange
        timeoutVar
        handle
        (encodeRpbDel request)
        decodeRpbDel))

deleteIndex ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbYokozunaIndexDeleteReq -- ^
  -> IO (Either HandleError (Either ByteString ()))
deleteIndex timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbYokozunaIndexDelete request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbDel
      (exchange
        timeoutVar
        handle
        (encodeRpbYokozunaIndexDelete request)
        decodeRpbDel))

get ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetResp))
get timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbGet request)

  doExchange
    (onReceive handlers)
    RespRpbGet
    (exchange
      timeoutVar
      handle
      (encodeRpbGet request)
      decodeRpbGet)

getBucket ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucket timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbGetBucket request)

  doExchange
    (onReceive handlers)
    RespRpbGetBucket
    (exchange
      timeoutVar
      handle
      (encodeRpbGetBucket request)
      decodeRpbGetBucket)

getBucketType ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucketType timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbGetBucketType request)

  doExchange
    (onReceive handlers)
    RespRpbGetBucket
    (exchange
      timeoutVar
      handle
      (encodeRpbGetBucketType request)
      decodeRpbGetBucket)

getCrdt ::
     TVar Bool -- ^
  -> Handle
  -> Proto.DtFetchReq
  -> IO (Either HandleError (Either ByteString Proto.DtFetchResp))
getCrdt timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqDtFetch request)

  doExchange
    (onReceive handlers)
    RespDtFetch
    (exchange
      timeoutVar
      handle
      (encodeDtFetch request)
      decodeDtFetch)

getIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbYokozunaIndexGet request)

  doExchange
    (onReceive handlers)
    RespRpbYokozunaIndexGet
    (exchange
      timeoutVar
      handle
      (encodeRpbYokozunaIndexGet request)
      decodeRpbYokozunaIndexGet)

getSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbYokozunaSchemaGet request)

  doExchange
    (onReceive handlers)
    RespRpbYokozunaSchemaGet
    (exchange
      timeoutVar
      handle
      (encodeRpbYokozunaSchemaGet request)
      decodeRpbYokozunaSchemaGet)

getServerInfo ::
     TVar Bool -- ^
  -> Handle
  -> IO (Either HandleError (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo timeoutVar handle@(Handle { handlers }) = do
  onSend handlers (ReqRpbGetServerInfo request)

  doExchange
    (onReceive handlers)
    RespRpbGetServerInfo
    (exchange
      timeoutVar
      handle
      (encodeRpbGetServerInfo request)
      decodeRpbGetServerInfo)

  where
    request :: Proto.RpbGetServerInfoReq
    request =
      Proto.defMessage

listBuckets ::
     TVar Bool
  -> Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either HandleError (Either ByteString r))
listBuckets timeoutVar handle@(Handle { handlers }) request responseFold = do
  onSend handlers (ReqRpbListBuckets request)

  doStream
    (onReceive handlers)
    RespRpbListBuckets
    (stream
      timeoutVar
      handle
      (encodeRpbListBuckets request)
      decodeRpbListBuckets)
    responseFold

doStream ::
     forall a r.
     (Response -> IO ())
  -> (a -> Response)
  -> (FoldM IO a r -> IO (Either HandleError (Either Proto.RpbErrorResp r)))
  -> FoldM IO a r
  -> IO (Either HandleError (Either ByteString r))
doStream onResponse toResponse action responseFold =
  action responseFold' >>= \case
    Left err ->
      pure (Left err)

    Right (Left response) -> do
      onResponse (RespRpbError response)
      pure (Right (Left (response ^. Proto.errmsg)))

    Right (Right result) ->
      pure (Right (Right result))

  where
    responseFold' :: FoldM IO a r
    responseFold' = do
      Foldl.premapM
        (\response -> do
          onResponse (toResponse response)
          pure response)
        responseFold

listKeys ::
     TVar Bool
  -> Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either HandleError (Either ByteString r))
listKeys timeoutVar handle@(Handle { handlers }) request responseFold = do
  onSend handlers (ReqRpbListKeys request)

  doStream
    (onReceive handlers)
    RespRpbListKeys
    (stream
      timeoutVar
      handle
      (encodeRpbListKeys request)
      decodeRpbListKeys)
    responseFold

mapReduce ::
     TVar Bool
  -> Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either HandleError (Either ByteString r))
mapReduce timeoutVar handle@(Handle { handlers }) request responseFold = do
  onSend handlers (ReqRpbMapRed request)

  doStream
    (onReceive handlers)
    RespRpbMapRed
    (stream
      timeoutVar
      handle
      (encodeRpbMapRed request)
      decodeRpbMapRed)
    responseFold

ping ::
     TVar Bool
  -> Handle
  -> IO (Either HandleError (Either ByteString ()))
ping timeoutVar handle@(Handle { handlers }) = do
  onSend handlers (ReqRpbPing request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbPing
      (exchange
        timeoutVar
        handle
        (encodeRpbPing request)
        decodeRpbPing))

  where
    request :: Proto.RpbPingReq
    request =
      Proto.defMessage

put ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either ByteString Proto.RpbPutResp))
put timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbPut request)

  doExchange
    (onReceive handlers)
    RespRpbPut
    (exchange
      timeoutVar
      handle
      (encodeRpbPut request)
      decodeRpbPut)

putIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either HandleError (Either ByteString ()))
putIndex timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbYokozunaIndexPut request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbPut
      (exchange
        timeoutVar
        handle
        (encodeRpbYokozunaIndexPut request)
        decodeRpbPut))

putSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either HandleError (Either ByteString ()))
putSchema timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbYokozunaSchemaPut request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbPut
      (exchange
        timeoutVar
        handle
        (encodeRpbYokozunaSchemaPut request)
        decodeRpbPut))

resetBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
resetBucket timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbResetBucket request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbResetBucket
      (exchange
        timeoutVar
        handle
        (encodeRpbResetBucket request)
        decodeRpbResetBucket))

setBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
setBucket timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbSetBucket request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbSetBucket
      (exchange
        timeoutVar
        handle
        (encodeRpbSetBucket request)
        decodeRpbSetBucket))

setBucketType ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either ByteString ()))
setBucketType timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbSetBucketType request)

  (fmap.fmap) (() <$)
    (doExchange
      (onReceive handlers)
      RespRpbSetBucket
      (exchange
        timeoutVar
        handle
        (encodeRpbSetBucketType request)
        decodeRpbSetBucket))

search ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either ByteString Proto.RpbSearchQueryResp))
search timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqRpbSearchQuery request)

  doExchange
    (onReceive handlers)
    RespRpbSearchQuery
    (exchange
      timeoutVar
      handle
      (encodeRpbSearchQuery request)
      decodeRpbSearchQuery)

secondaryIndex ::
     TVar Bool
  -> Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either HandleError (Either ByteString r))
secondaryIndex timeoutVar handle@(Handle { handlers }) request responseFold = do
  onSend handlers (ReqRpbIndex request)

  doStream
    (onReceive handlers)
    RespRpbIndex
    (stream
      timeoutVar
      handle
      (encodeRpbIndex request)
      decodeRpbIndex)
    responseFold

updateCrdt ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.DtUpdateResp))
updateCrdt timeoutVar handle@(Handle { handlers }) request = do
  onSend handlers (ReqDtUpdate request)

  doExchange
    (onReceive handlers)
    RespDtUpdate
    (exchange
      timeoutVar
      handle
      (encodeDtUpdate request)
      decodeDtUpdate)
