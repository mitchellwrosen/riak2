{-# LANGUAGE CPP #-}

module Libriak.Handle
  ( Handle
  , HandleError(..)
  , connect
  , softDisconnect
  , hardDisconnect
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
import Data.Function                      (on)
import Data.Kind                          (Type)
import Data.NF                            (NF, getNF, makeNF)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Socket.Stream.IPv4                 (CloseException, ConnectException(..),
                                           Endpoint(..), Interruptibility(..))

import qualified Data.Riak.Proto as Proto


data Handle
  = Handle
  { connection :: Connection
  , stateVar :: TVar State
  , sendAsync :: Async ()
  , receiveAsync :: Async ()
  , queue :: TBQueue Item
  }

instance Eq Handle where
  (==) =
    (==) `on` stateVar

data State
  -- | The socket is connected and hasn't failed yet. Continue servicing
  -- requests as normal.
  = Healthy
  -- | Either a send failed, receive failed, or protobuf decode failed; in all
  -- cases, the socket is deemed unusable. The only thing left to do is close
  -- the file descriptor.
  | Unhealthy
  -- | The socket is (or is just about to be) disconnected, and its file
  -- descriptor freed. The "hardness" is a useful signal to the background send
  -- and receive threads, which are interested in knowing if they should
  -- continue processing the already-enqueued requests or not.
  | Disconnected Hardness

data Hardness
  -- | Stop accepting new requests, but do finish outstanding ones. The TVar
  -- within is set to True by the send thread when all requests have been sent,
  -- so the receive thread knows when to stop receiving.
  = Soft (TVar Bool)
  -- | Same, but don't bother finishing outstanding ones
  | Hard

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

-- | Acquire a handle.
--
-- This function must be called with asynchronous exceptions masked, and the
-- caller must eventually call 'disconnect'.
--
-- /Throws/: This function will never throw an exception.
connect ::
     TVar Bool
  -> Endpoint
  -> IO (Either (ConnectException 'Interruptible) Handle)
connect timeoutVar endpoint =
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
        })

sendThread ::
     TVar State
  -> TVar Bool
  -> TBQueue Item
  -> TQueue SentItem
  -> Connection
  -> IO ()
sendThread stateVar streamingVar itemQueue sentItemQueue connection =
  healthyLoop

  where
    healthyLoop :: IO ()
    healthyLoop =
      join . atomically $
        readTVar stateVar >>= \case
          Healthy ->
            sendItem healthyLoop <$> readTBQueue itemQueue

          -- Don't know if it's a hard or soft disconnect yet, so wait around
          -- for 'disconnect' to be called.
          Unhealthy ->
            retry

          Disconnected (Soft doneVar) ->
            pure (flushingLoop doneVar)

          Disconnected Hard ->
            pure (pure ())

    flushingLoop :: TVar Bool -> IO ()
    flushingLoop doneVar =
      join . atomically $
        (sendItem (flushingLoop doneVar) <$> readTBQueue itemQueue)
        <|>
        (do
          writeTVar doneVar True
          pure (pure ()))

    sendItem :: IO () -> Item -> IO ()
    sendItem next = \case
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
            next

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
              (healthyLoop <$ blockUntilFalse streamingVar)
              <|>
              (readTVar stateVar >>= \case
                Healthy ->
                  retry

                Unhealthy ->
                  retry

                Disconnected (Soft doneVar) ->
                  pure $
                    join . atomically $
                      (flushingLoop doneVar <$ blockUntilFalse streamingVar)

                Disconnected Hard ->
                  pure (pure ()))

blockUntilFalse :: TVar Bool -> STM ()
blockUntilFalse var =
  readTVar var >>= \case
    True -> retry
    False -> pure ()

receiveThread ::
     TVar State
  -> TVar Bool
  -> TQueue SentItem
  -> Connection
  -> IO ()
receiveThread stateVar streamingVar sentItemQueue connection =
  healthyLoop

  where
    healthyLoop :: IO ()
    healthyLoop =
      join . atomically $
        readTVar stateVar >>= \case
          Healthy ->
            handleItem healthyLoop <$> readTQueue sentItemQueue

          Unhealthy ->
            retry

          Disconnected (Soft doneVar) ->
            pure (flushingLoop doneVar)

          Disconnected Hard ->
            pure (pure ())

    flushingLoop :: TVar Bool -> IO ()
    flushingLoop doneVar =
      join . atomically $
        (handleItem (flushingLoop doneVar) <$> readTQueue sentItemQueue)
        <|>
        (readTVar doneVar >>= \case
          False -> retry
          True -> pure (pure ()))

    handleItem :: IO () -> SentItem -> IO ()
    handleItem next = \case
      Exchange' timeoutVar responseVar ->
        Connection.receive timeoutVar connection >>= \case
          Left err ->
            atomically $ do
              putTMVar responseVar (Left (HandleConnectionError err))
              writeTVar stateVar Unhealthy

          Right response -> do
            atomically (putTMVar responseVar (Right response))
            next

      Stream' timeoutVar decodeResponse responseQueue -> do
        streamResponse next timeoutVar decodeResponse responseQueue

    streamResponse ::
         forall response.
         HasLens' response "done" Bool
      => IO ()
      -> TVar Bool
      -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
      -> TQueue (Either HandleError (Either Proto.RpbErrorResp response))
      -> IO ()
    streamResponse next timeoutVar decodeResponse responseQueue =
      receiveLoop

      where
        receiveLoop :: IO ()
        receiveLoop =
          Connection.receive timeoutVar connection >>= \case
            Left err ->
              atomically $ do
                writeTQueue responseQueue (Left (HandleConnectionError err))
                healthyToUnhealthy stateVar

            Right response ->
              case decodeResponse response of
                Left err ->
                  -- TODO Hrm, thread that called 'stream' might see
                  -- Disconnected before its own decode error... ;(
                  atomically $ do
                    writeTQueue responseQueue (Left (HandleDecodeError err))
                    healthyToUnhealthy stateVar

                Right (Left response) -> do
                  atomically $ do
                    writeTQueue responseQueue (Right (Left response))
                    writeTVar streamingVar False
                  next

                Right (Right response) -> do
                  atomically (writeTQueue responseQueue (Right (Right response)))

                  if response ^. Proto.done
                    then do
                      atomically (writeTVar streamingVar False)
                      next
                    else
                      receiveLoop

healthyToUnhealthy :: TVar State -> STM ()
healthyToUnhealthy stateVar =
  readTVar stateVar >>= \case
    Healthy -> writeTVar stateVar Unhealthy
    Unhealthy -> pure ()
    Disconnected _ -> pure ()

-- | Wait for all in-flight requests to complete, then disconnect a handle.
-- Idempotent.
--
-- /Throws/: This function will never throw an exception.
softDisconnect ::
     Handle
  -> IO (Either CloseException ())
softDisconnect handle@Handle { stateVar } = do
  doneVar :: TVar Bool <-
    newTVarIO False

  join . atomically $
    readTVar stateVar >>= \case
      Healthy -> disconnect handle (Soft doneVar)
      Unhealthy -> disconnect handle (Soft doneVar)
      Disconnected _ -> pure (pure (Right ()))

-- | Disconnect a handle, regardless of if there are any in-flight requests.
-- Idempotent.
--
-- /Throws/: This function will never throw an exception.
hardDisconnect ::
     Handle
  -> IO (Either CloseException ())
hardDisconnect handle@Handle { stateVar } =
  join . atomically $
    readTVar stateVar >>= \case
      Healthy -> disconnect handle Hard
      Unhealthy -> disconnect handle Hard
      Disconnected _ -> pure (pure (Right ()))

disconnect :: Handle -> Hardness -> STM (IO (Either CloseException ()))
disconnect
    Handle { connection, receiveAsync, sendAsync, stateVar }
    hardness = do

  writeTVar stateVar (Disconnected hardness)

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
        (readTVar stateVar >>= \case
          Disconnected Hard ->
            pure (pure (Left HandleClosedError))

          Healthy               -> retry
          Unhealthy             -> retry
          Disconnected (Soft _) -> retry)

    Unhealthy      -> pure (Left HandleClosedError)
    Disconnected _ -> pure (Left HandleClosedError)


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

    Unhealthy      -> pure (Left HandleClosedError)
    Disconnected _ -> pure (Left HandleClosedError)

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
            (readTVar stateVar >>= \case
              Disconnected Hard ->
                pure (pure (Left HandleClosedError))

              Healthy               -> retry
              Unhealthy             -> retry
              Disconnected (Soft _) -> retry)

--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

delete ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbDelReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbDelResp))
delete timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbDel request)
    decodeRpbDel

deleteIndex ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbYokozunaIndexDeleteReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbDelResp))
deleteIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaIndexDelete request)
    decodeRpbDel

get ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbGetResp))
get timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbGet request)
    decodeRpbGet

getBucket ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbGetBucketResp))
getBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbGetBucket request)
    decodeRpbGetBucket

getBucketType ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbGetBucketResp))
getBucketType timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbGetBucketType request)
    decodeRpbGetBucket

getCrdt ::
     TVar Bool -- ^
  -> Handle
  -> Proto.DtFetchReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.DtFetchResp))
getCrdt timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeDtFetch request)
    decodeDtFetch

getIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbYokozunaIndexGetResp))
getIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaIndexGet request)
    decodeRpbYokozunaIndexGet

getSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbYokozunaSchemaGetResp))
getSchema timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaSchemaGet request)
    decodeRpbYokozunaSchemaGet

getServerInfo ::
     TVar Bool -- ^
  -> Handle
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbGetServerInfoResp))
getServerInfo timeoutVar handle =
  exchange
    timeoutVar
    handle
    (encodeRpbGetServerInfo Proto.defMessage)
    decodeRpbGetServerInfo

listBuckets ::
     TVar Bool
  -> Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either HandleError (Either Proto.RpbErrorResp r))
listBuckets timeoutVar handle request =
  stream
    timeoutVar
    handle
    (encodeRpbListBuckets request)
    decodeRpbListBuckets

listKeys ::
     TVar Bool
  -> Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either HandleError (Either Proto.RpbErrorResp r))
listKeys timeoutVar handle request =
  stream
    timeoutVar
    handle
    (encodeRpbListKeys request)
    decodeRpbListKeys

mapReduce ::
     TVar Bool
  -> Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either HandleError (Either Proto.RpbErrorResp r))
mapReduce timeoutVar handle request =
  stream
    timeoutVar
    handle
    (encodeRpbMapRed request)
    decodeRpbMapRed

ping ::
     TVar Bool
  -> Handle
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbPingResp))
ping timeoutVar handle =
  exchange
    timeoutVar
    handle
    (encodeRpbPing Proto.defMessage)
    decodeRpbPing

put ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbPutResp))
put timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbPut request)
    decodeRpbPut

putIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbPutResp))
putIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaIndexPut request)
    decodeRpbPut

putSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbPutResp))
putSchema timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaSchemaPut request)
    decodeRpbPut

resetBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbResetBucketResp))
resetBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbResetBucket request)
    decodeRpbResetBucket

setBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbSetBucketResp))
setBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbSetBucket request)
    decodeRpbSetBucket

setBucketType ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbSetBucketResp))
setBucketType timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbSetBucketType request)
    decodeRpbSetBucket

search ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.RpbSearchQueryResp))
search timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbSearchQuery request)
    decodeRpbSearchQuery

secondaryIndex ::
     TVar Bool
  -> Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either HandleError (Either Proto.RpbErrorResp r))
secondaryIndex timeoutVar handle request =
  stream
    timeoutVar
    handle
    (encodeRpbIndex request)
    decodeRpbIndex

updateCrdt ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either HandleError (Either Proto.RpbErrorResp Proto.DtUpdateResp))
updateCrdt timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeDtUpdate request)
    decodeDtUpdate
