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
import Libriak.Request    (Request(..), encodeRequest)
import Libriak.Response   (DecodeError, Response(..), decodeResponse,
                           responseDone)

import qualified Libriak.Connection as Connection

import Control.Applicative     ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe  (SomeException, catchAsync, throwIO)
import Control.Foldl           (FoldM(..))
import Control.Lens            ((^.))
import Data.ByteString         (ByteString)
import Data.Function           (on)
import Data.IORef              (IORef, newIORef, readIORef, writeIORef)
import Data.Kind               (Type)
import Data.Profunctor         (lmap)
import GHC.TypeLits            (KnownNat)
import Socket.Stream.IPv4      (CloseException(..), ConnectException(..),
                                Endpoint(..), Interruptibility(..))

import qualified Data.Riak.Proto as Proto


data Handle
  = Handle
  { connVar :: TVar (Either Connection Connection)
    -- ^ The connection. Starts out on the Right, but once a connection error
    -- occurs, gets put on the Left permanently.
  , sendLock :: MVar ()
    -- ^ Lock acquired during sending a request.
  , doneVarRef :: IORef (TMVar ())
  , receiveTimeout :: Int
  , handlers :: EventHandlers
  }

instance Eq Handle where
  (==) =
    (==) `on` sendLock

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
  { onSend :: forall code. Request code -> IO ()
    -- ^ Called just prior to sending a request.
  , onReceive :: forall code. Response code -> IO ()
    -- ^ Called just after receiving a response.
  , onError :: HandleError -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 <> EventHandlers a2 b2 c2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
connect ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO (Either (ConnectException 'Uninterruptible) Handle)
connect endpoint receiveTimeout handlers =
  Connection.connect endpoint >>= \case
    Left err ->
      pure (Left err)

    Right connection -> do
      connVar :: TVar (Either Connection Connection) <-
        newTVarIO (Right connection)

      sendLock :: MVar () <-
        newMVar ()

      doneVarRef :: IORef (TMVar ()) <-
        newIORef =<< newTMVarIO ()

      pure (Right Handle
        { connVar = connVar
        , sendLock = sendLock
        , doneVarRef = doneVarRef
        , receiveTimeout = receiveTimeout
        , handlers = handlers
        })

disconnect ::
     Handle
  -> IO (Either CloseException ())
disconnect Handle { connVar } =
  readTVarIO connVar >>=
    either Connection.disconnect Connection.disconnect

-- | Send a request and receive the response (a single message).
exchange ::
     KnownNat code
  => Handle -- ^
  -> Request code -- ^
  -> (Response code -> a) -- ^
  -> IO (Either HandleError (Either ByteString a))
exchange
    handle@(Handle { connVar, doneVarRef, handlers, receiveTimeout, sendLock })
    request
    onSuccess =

  withConnection handle $ \connection -> do
    -- Try sending, which either results in an error, or two empty TMVars: one
    -- that will fill when it's our turn to receive, and one that we must fill
    -- when we are done receiving.
    sendResult :: Either HandleError (TMVar (), TMVar ()) <-
      withMVar sendLock $ \() ->
        send connection request handlers >>= \case
          Left err ->
            pure (Left (HandleConnectionError err))

          Right () -> do
            -- TODO Bug here, if we crash before executing this swap the next
            -- thread that comes along will receive our response
            doneVar <- newEmptyTMVarIO
            prevDoneVar <- readIORef doneVarRef
            writeIORef doneVarRef doneVar
            pure (Right (prevDoneVar, doneVar))

    case sendResult of
      Left err ->
        pure (Left err)

      Right (prevDoneVar, doneVar) -> do
        -- It's a race: either a previous request fails, or everything goes well
        -- and it finally becomes our turn to receive.
        waitResult :: Maybe HandleError <-
          atomically $ do
            (Nothing <$ readTMVar prevDoneVar)
            <|>
            (readTVar connVar >>= \case
              Left _ -> pure (Just HandleClosedError)
              Right _ -> retry)

        case waitResult of
          Nothing ->
            receive connection receiveTimeout handlers >>= \case
              Left err ->
                pure (Left err)

              Right response -> do
                atomically (putTMVar doneVar ())
                pure (Right (onSuccess <$> response))

          Just err ->
            pure (Left err)

-- | Send a request and stream the response (one or more messages).
stream ::
     forall code r.
     KnownNat code
  => Handle -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either HandleError (Either ByteString r))
stream
    handle@(Handle { handlers, receiveTimeout, sendLock })
    request
    (FoldM step (initial :: IO x) extract) =

  withConnection handle $ \connection ->
    -- Riak request handling state machine is odd. Streaming responses are
    -- special; when one is active, no other requests can be serviced on this
    -- socket.
    --
    -- So, hold a lock for the entirety of the request-response exchange, not
    -- just during sending the request.
    withMVar sendLock $ \() ->
      send connection request handlers >>= \case
        Left err ->
          pure (Left (HandleConnectionError err))

        Right () ->
          let
            consume :: x -> IO (Either HandleError (Either ByteString r))
            consume value =
              receive connection receiveTimeout handlers >>= \case
                Left err ->
                  pure (Left err)

                Right (Left err) ->
                  pure (Right (Left err))

                Right (Right response) -> do
                  newValue <- step value response
                  if responseDone response
                    then Right . Right <$> extract newValue
                    else consume newValue
          in
            consume =<< initial


--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

delete ::
     Handle -- ^
  -> Proto.RpbDelReq
  -> IO (Either HandleError (Either ByteString ()))
delete handle request =
  exchange
    handle
    (ReqRpbDel request)
    (\(RespRpbDel _) -> ())

deleteIndex ::
     Handle
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either HandleError (Either ByteString ()))
deleteIndex handle request =
  exchange
    handle
    (ReqRpbYokozunaIndexDelete request)
    (\(RespRpbDel _) -> ())

get ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbGetResp))
get handle request =
  exchange
    handle
    (ReqRpbGet request)
    (\(RespRpbGet response) -> response)

getBucket ::
     Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucket handle request =
  exchange
    handle
    (ReqRpbGetBucket request)
    (\(RespRpbGetBucket response) -> response)

getBucketType ::
     Handle -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucketType handle request =
  exchange
    handle
    (ReqRpbGetBucketType request)
    (\(RespRpbGetBucket response) -> response)

getCrdt ::
     Handle
  -> Proto.DtFetchReq
  -> IO (Either HandleError (Either ByteString Proto.DtFetchResp))
getCrdt handle request =
  exchange
    handle
    (ReqDtFetch request)
    (\(RespDtFetch response) -> response)

getIndex ::
     Handle
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex handle request =
  exchange
    handle
    (ReqRpbYokozunaIndexGet request)
    (\(RespRpbYokozunaIndexGet response) -> response)

getSchema ::
     Handle
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema handle request =
  exchange
    handle
    (ReqRpbYokozunaSchemaGet request)
    (\(RespRpbYokozunaSchemaGet response) -> response)

getServerInfo ::
     Handle
  -> IO (Either HandleError (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo handle =
  exchange
    handle
    (ReqRpbGetServerInfo Proto.defMessage)
    (\(RespRpbGetServerInfo response) -> response)

listBuckets ::
     Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either HandleError (Either ByteString r))
listBuckets handle request responseFold =
  stream
    handle
    (ReqRpbListBuckets request)
    (lmap (\(RespRpbListBuckets response) -> response) responseFold)

listKeys ::
     Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either HandleError (Either ByteString r))
listKeys handle request responseFold =
  stream
    handle
    (ReqRpbListKeys request)
    (lmap (\(RespRpbListKeys response) -> response) responseFold)

mapReduce ::
     Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either HandleError (Either ByteString r))
mapReduce handle request responseFold =
  stream
    handle
    (ReqRpbMapRed request)
    (lmap (\(RespRpbMapRed response) -> response) responseFold)

ping ::
     Handle
  -> IO (Either HandleError (Either ByteString ()))
ping handle =
  exchange
    handle
    (ReqRpbPing Proto.defMessage)
    (\(RespRpbPing _) -> ())

put ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either ByteString Proto.RpbPutResp))
put handle request =
  exchange
    handle
    (ReqRpbPut request)
    (\(RespRpbPut response) -> response)

putIndex ::
     Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either HandleError (Either ByteString ()))
putIndex handle request =
  exchange
    handle
    (ReqRpbYokozunaIndexPut request)
    (\(RespRpbPut _) -> ())

putSchema ::
     Handle
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either HandleError (Either ByteString ()))
putSchema handle request =
  exchange
    handle
    (ReqRpbYokozunaSchemaPut request)
    (\(RespRpbPut _) -> ())

resetBucket ::
     Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
resetBucket handle request =
  exchange
    handle
    (ReqRpbResetBucket request)
    (\(RespRpbResetBucket _) -> ())

setBucket ::
     Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
setBucket handle request =
  exchange
    handle
    (ReqRpbSetBucket request)
    (\(RespRpbSetBucket _) -> ())

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either ByteString ()))
setBucketType handle request =
  exchange
    handle
    (ReqRpbSetBucketType request)
    (\(RespRpbSetBucket _) -> ())

search ::
     Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either ByteString Proto.RpbSearchQueryResp))
search handle request =
  exchange
    handle
    (ReqRpbSearchQuery request)
    (\(RespRpbSearchQuery response) -> response)

secondaryIndex ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either HandleError (Either ByteString r))
secondaryIndex handle request responseFold =
  stream
    handle
    (ReqRpbIndex request)
    (lmap (\(RespRpbIndex response) -> response) responseFold)

updateCrdt ::
     Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.DtUpdateResp))
updateCrdt handle request =
  exchange
    handle
    (ReqDtUpdate request)
    (\(RespDtUpdate response) -> response)


withConnection ::
     forall a.
     Handle
  -> (Connection -> IO (Either HandleError a))
  -> IO (Either HandleError a)
withConnection Handle { connVar, handlers } callback =
  readTVarIO connVar >>= \case
    Left _ ->
      pure (Left HandleClosedError)

    Right connection ->
      doCallback connection >>= \case
        Left err -> do
          markAsUnusable
          onError handlers err
          pure (Left err)

        Right result ->
          pure (Right result)

  where
    doCallback :: Connection -> IO (Either HandleError a)
    doCallback connection =
      callback connection `catchAsync` \ex -> do
        markAsUnusable
        throwIO (ex :: SomeException)

    markAsUnusable :: IO ()
    markAsUnusable =
      atomically $
        readTVar connVar >>= \case
          Left _ -> pure ()
          Right conn -> writeTVar connVar (Left conn)

send ::
     Connection
  -> Request code
  -> EventHandlers
  -> IO (Either ConnectionError ())
send connection request handlers = do
  onSend handlers request
  Connection.send connection (encodeRequest request)

receive ::
     forall code.
     KnownNat code
  => Connection
  -> Int
  -> EventHandlers
  -> IO (Either HandleError (Either ByteString (Response code)))
receive connection timeout handlers = do
  Connection.receive connection timeout >>= \case
    Left err ->
      pure (Left (HandleConnectionError err))

    Right bytes ->
      case decodeResponse bytes of
        Left err -> do
          pure (Left (HandleDecodeError err))

        Right (Left response) -> do
          onReceive handlers response
          case response of
            RespRpbError resp ->
              pure (Right (Left (resp ^. Proto.errmsg)))

        Right (Right response) -> do
          onReceive handlers response
          pure (Right (Right response))

