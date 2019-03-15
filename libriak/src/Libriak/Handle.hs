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
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar            (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe             (SomeException, catchAsync, throwIO)
import Control.Foldl                      (FoldM(..))
import Control.Lens                       ((^.))
import Control.Monad                      (join)
import Data.ByteString                    (ByteString)
import Data.Function                      (on)
import Data.IORef                         (IORef, newIORef, readIORef,
                                           writeIORef)
import Data.Kind                          (Type)
import Data.NF                            (NF, getNF, makeNF)
import Data.Profunctor                    (lmap)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Data.Proxy
import GHC.TypeLits                       (KnownNat, sameNat)
import Socket.Stream.IPv4                 (CloseException(..),
                                           ConnectException(..), Endpoint(..),
                                           Interruptibility(..))

import qualified Data.Riak.Proto as Proto


-- TODO use Chan instead of TBQueue/TQueue in Libriak.Handle?

data Handle
  = Handle
  { stateVar :: TVar State
  , queue :: TBQueue Item
  , sendAsync :: Async ()
  , receiveAsync :: Async ()
  , handlers :: EventHandlers
  }

instance Eq Handle where
  (==) = undefined

data State
  = Connected
  | Disconnected

data Item :: Type where
  Exchange ::
       NF EncodedRequest
    -> TMVar (Either HandleError EncodedResponse)
    -> Item

  Stream ::
       NF EncodedRequest
    -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
    -> TQueue (Either HandleError response)
    -> Item

data SentItem :: Type where
  Exchange' ::
       TMVar (Either HandleError EncodedResponse)
    -> SentItem

  Stream' ::
       (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
    -> TQueue (Either HandleError response)
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
  -- { onSend :: Request -> IO ()
  --   -- ^ Called just prior to enqueueing a request to be sent.
  -- , onReceive :: Response -> IO ()
  --   -- ^ Called just after receiving a response.
  -- -- , onError :: HandleError -> IO ()
  -- }

-- instance Monoid EventHandlers where
--   mempty = EventHandlers mempty mempty -- mempty
--   mappend = (<>)

-- instance Semigroup EventHandlers where
--   EventHandlers a1 b1 <> EventHandlers a2 b2 =
--     EventHandlers (a1 <> a2) (b1 <> b2) -- (c1 <> c2)

-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
connect ::
     Endpoint
  -> EventHandlers
  -> IO (Either (ConnectException 'Uninterruptible) Handle)
connect endpoint handlers =
  Connection.connect endpoint >>= \case
    Left err ->
      pure (Left err)

    Right connection -> do
      stateVar :: TVar State <-
        newTVarIO Connected

      streamingVar :: TVar Bool <-
        newTVarIO False

      itemQueue :: TBQueue Item <-
        newTBQueueIO (2^(14::Int))

      sentItemQueue :: TQueue SentItem <-
        newTQueueIO

      sendAsync <-
        async
          (sendThread
            stateVar
            streamingVar
            itemQueue
            sentItemQueue
            connection)

      receiveAsync <-
        async
          undefined

      -- Close connection!
      -- Fork recv thread!

      pure (Right Handle
        { stateVar = stateVar
        , queue = itemQueue
        , sendAsync = sendAsync
        , receiveAsync = receiveAsync
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
      join (atomically (processItem <|> stopIfDisconnected))

    processItem :: STM (IO ())
    processItem =
      sendItem <$>
        readTBQueue itemQueue

    sendItem :: Item -> IO ()
    sendItem = \case
      Exchange request responseVar ->
        Connection.send connection (getNF request) >>= \case
          Left err -> do
            atomically (writeTVar stateVar Disconnected)

            atomically
              (putTMVar
                responseVar
                (Left (HandleConnectionError err)))

          Right () -> do
            atomically (writeTQueue sentItemQueue (Exchange' responseVar))

            loop

      Stream request parseResponse responseQueue ->
        Connection.send connection (getNF request) >>= \case
          Left err -> do
            atomically (writeTVar stateVar Disconnected)

            atomically
              (writeTQueue
                responseQueue
                (Left (HandleConnectionError err)))

          Right () -> do
            atomically $ do
              writeTVar streamingVar True

              writeTQueue
                sentItemQueue
                (Stream'
                  parseResponse
                  responseQueue)

            join . atomically $
              (readTVar streamingVar >>= \case
                True -> retry
                False -> pure loop)
              <|>
              stopIfDisconnected

    stopIfDisconnected :: STM (IO ())
    stopIfDisconnected =
      readTVar stateVar >>= \case
        Connected -> retry
        Disconnected -> pure (pure ())

-- | Disconnect a handle if it's not already disconnected.
--
-- /Throws/: This function will never throw an exception.
disconnect ::
     Handle
  -> IO (Either CloseException ())
disconnect Handle { stateVar } =
  join . atomically $
    readTVar stateVar >>= \case

    writeTVar stateVar Disconnected)

-- | Send a request and receive the response (a single message).
exchange ::
     forall response.
     TVar Bool
  -> Handle
  -> EncodedRequest
  -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
  -> IO (Either HandleError (Either ByteString response))
exchange
    timeoutVar
    handle@(Handle { queue, stateVar })
    request
    fromResponse =

  readTVarIO stateVar >>= \case
    Disconnected ->
      pure (Left HandleClosedError)

    Connected -> do
      responseVar :: TMVar (Either HandleError EncodedResponse) <-
        newEmptyTMVarIO

      atomically
        (writeTBQueue
          queue
          (Exchange (makeNF request) responseVar))

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
                    disconnect handle
                    pure (Left (HandleDecodeError err))

                  Right (Left response) ->
                    pure (Right (Left (response ^. Proto.errmsg)))

                  Right (Right response) ->
                    pure (Right (Right response)))
        <|>
        (do
          readTVar stateVar >>= \case
            Connected ->
              retry

            Disconnected ->
              pure (pure (Left HandleClosedError)))

-- | Send a request and stream the response (one or more messages).
stream ::
     forall r response.
     HasLens' response "done" Bool
  => TVar Bool
  -> Handle -- ^
  -> EncodedRequest -- ^
  -> (EncodedResponse -> Either DecodeError (Either Proto.RpbErrorResp response))
  -> FoldM IO response r
  -> IO (Either HandleError (Either ByteString r))
stream
    timeoutVar
    handle@(Handle { handlers, queue, stateVar })
    request
    parseResponse
    (FoldM step (initial :: IO x) extract) =

  readTVarIO stateVar >>= \case
    Disconnected ->
      pure (Left HandleClosedError)

    Connected -> do
      responseQueue :: TQueue (Either HandleError response) <-
        newTQueueIO

      atomically
        (writeTBQueue
          queue
          (Stream
            (makeNF request)
            parseResponse
            responseQueue))

      processResponse responseQueue

  where
    processResponse ::
         TQueue (Either HandleError response)
      -> IO (Either HandleError (Either ByteString r))
    processResponse responseQueue =
      initial >>= loop

      where
        loop :: x -> IO (Either HandleError (Either ByteString r))
        loop acc =
          join . atomically $
            (do
              response :: Either HandleError response <-
                readTQueue responseQueue

              pure $
                case response of
                  Left err ->
                    pure (Left err)

                  Right response -> do
                    acc' :: x <-
                      step acc response

                    if response ^. Proto.done
                      then
                        Right . Right <$>
                          extract acc'

                      else
                        loop acc')
            <|>
            (do
              readTVar stateVar >>= \case
                Connected ->
                  retry

                Disconnected ->
                  pure (pure (Left HandleClosedError)))


--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

delete ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbDelReq -- ^
  -> IO (Either HandleError (Either ByteString ()))
delete timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbDel request)
      decodeRpbDel)

deleteIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either HandleError (Either ByteString ()))
deleteIndex timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbYokozunaIndexDelete request)
      decodeRpbDel)

get ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbGetResp))
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
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
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
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
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
  -> IO (Either HandleError (Either ByteString Proto.DtFetchResp))
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
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaIndexGetResp))
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
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeRpbYokozunaSchemaGet request)
    decodeRpbYokozunaSchemaGet

getServerInfo ::
     TVar Bool -- ^
  -> Handle
  -> IO (Either HandleError (Either ByteString Proto.RpbGetServerInfoResp))
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
  -> IO (Either HandleError (Either ByteString r))
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
  -> IO (Either HandleError (Either ByteString r))
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
  -> IO (Either HandleError (Either ByteString r))
mapReduce timeoutVar handle request =
  stream
    timeoutVar
    handle
    (encodeRpbMapRed request)
    decodeRpbMapRed

ping ::
     TVar Bool
  -> Handle
  -> IO (Either HandleError (Either ByteString ()))
ping timeoutVar handle =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbPing Proto.defMessage)
      decodeRpbPing)

put ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either ByteString Proto.RpbPutResp))
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
  -> IO (Either HandleError (Either ByteString ()))
putIndex timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbYokozunaIndexPut request)
      decodeRpbPut)

putSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either HandleError (Either ByteString ()))
putSchema timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbYokozunaSchemaPut request)
      decodeRpbPut)

resetBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
resetBucket timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbResetBucket request)
      decodeRpbResetBucket)

setBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
setBucket timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbSetBucket request)
      decodeRpbSetBucket)

setBucketType ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either ByteString ()))
setBucketType timeoutVar handle request =
  (fmap.fmap) (() <$)
    (exchange
      timeoutVar
      handle
      (encodeRpbSetBucketType request)
      decodeRpbSetBucket)

search ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either ByteString Proto.RpbSearchQueryResp))
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
  -> IO (Either HandleError (Either ByteString r))
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
  -> IO (Either HandleError (Either ByteString Proto.DtUpdateResp))
updateCrdt timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (encodeDtUpdate request)
    decodeDtUpdate
