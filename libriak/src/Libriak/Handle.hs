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
  -> EventHandlers
  -> IO (Either (ConnectException 'Uninterruptible) Handle)
connect endpoint handlers =
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
  => TVar Bool
  -> Handle -- ^
  -> Request code -- ^
  -> (Response code -> a) -- ^
  -> IO (Either HandleError (Either ByteString a))
exchange
    timeoutVar
    handle@(Handle { connVar, doneVarRef, handlers, sendLock })
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
            receive timeoutVar connection handlers >>= \case
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
  => TVar Bool
  -> Handle -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either HandleError (Either ByteString r))
stream
    timeoutVar
    handle@(Handle { handlers, sendLock })
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
              receive timeoutVar connection handlers >>= \case
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
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbDelReq -- ^
  -> IO (Either HandleError (Either ByteString ()))
delete timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbDel request)
    (\(RespRpbDel _) -> ())

deleteIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexDeleteReq
  -> IO (Either HandleError (Either ByteString ()))
deleteIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbYokozunaIndexDelete request)
    (\(RespRpbDel _) -> ())

get ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbGetResp))
get timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbGet request)
    (\(RespRpbGet response) -> response)

getBucket ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbGetBucket request)
    (\(RespRpbGetBucket response) -> response)

getBucketType ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.RpbGetBucketTypeReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.RpbGetBucketResp))
getBucketType timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbGetBucketType request)
    (\(RespRpbGetBucket response) -> response)

getCrdt ::
     TVar Bool -- ^
  -> Handle
  -> Proto.DtFetchReq
  -> IO (Either HandleError (Either ByteString Proto.DtFetchResp))
getCrdt timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqDtFetch request)
    (\(RespDtFetch response) -> response)

getIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbYokozunaIndexGet request)
    (\(RespRpbYokozunaIndexGet response) -> response)

getSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaGetReq
  -> IO (Either HandleError (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbYokozunaSchemaGet request)
    (\(RespRpbYokozunaSchemaGet response) -> response)

getServerInfo ::
     TVar Bool -- ^
  -> Handle
  -> IO (Either HandleError (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo timeoutVar handle =
  exchange
    timeoutVar
    handle
    (ReqRpbGetServerInfo Proto.defMessage)
    (\(RespRpbGetServerInfo response) -> response)

listBuckets ::
     TVar Bool
  -> Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either HandleError (Either ByteString r))
listBuckets timeoutVar handle request responseFold =
  stream
    timeoutVar
    handle
    (ReqRpbListBuckets request)
    (lmap (\(RespRpbListBuckets response) -> response) responseFold)

listKeys ::
     TVar Bool
  -> Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either HandleError (Either ByteString r))
listKeys timeoutVar handle request responseFold =
  stream
    timeoutVar
    handle
    (ReqRpbListKeys request)
    (lmap (\(RespRpbListKeys response) -> response) responseFold)

mapReduce ::
     TVar Bool
  -> Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either HandleError (Either ByteString r))
mapReduce timeoutVar handle request responseFold =
  stream
    timeoutVar
    handle
    (ReqRpbMapRed request)
    (lmap (\(RespRpbMapRed response) -> response) responseFold)

ping ::
     TVar Bool
  -> Handle
  -> IO (Either HandleError (Either ByteString ()))
ping timeoutVar handle =
  exchange
    timeoutVar
    handle
    (ReqRpbPing Proto.defMessage)
    (\(RespRpbPing _) -> ())

put ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either ByteString Proto.RpbPutResp))
put timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbPut request)
    (\(RespRpbPut response) -> response)

putIndex ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either HandleError (Either ByteString ()))
putIndex timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbYokozunaIndexPut request)
    (\(RespRpbPut _) -> ())

putSchema ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbYokozunaSchemaPutReq
  -> IO (Either HandleError (Either ByteString ()))
putSchema timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbYokozunaSchemaPut request)
    (\(RespRpbPut _) -> ())

resetBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
resetBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbResetBucket request)
    (\(RespRpbResetBucket _) -> ())

setBucket ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either ByteString ()))
setBucket timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbSetBucket request)
    (\(RespRpbSetBucket _) -> ())

setBucketType ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either ByteString ()))
setBucketType timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbSetBucketType request)
    (\(RespRpbSetBucket _) -> ())

search ::
     TVar Bool -- ^
  -> Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either ByteString Proto.RpbSearchQueryResp))
search timeoutVar handle request =
  exchange
    timeoutVar
    handle
    (ReqRpbSearchQuery request)
    (\(RespRpbSearchQuery response) -> response)

secondaryIndex ::
     TVar Bool
  -> Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either HandleError (Either ByteString r))
secondaryIndex timeoutVar handle request responseFold =
  stream
    timeoutVar
    handle
    (ReqRpbIndex request)
    (lmap (\(RespRpbIndex response) -> response) responseFold)

updateCrdt ::
     TVar Bool -- ^
  -> Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either HandleError (Either ByteString Proto.DtUpdateResp))
updateCrdt timeoutVar handle request =
  exchange
    timeoutVar
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
  => TVar Bool
  -> Connection
  -> EventHandlers
  -> IO (Either HandleError (Either ByteString (Response code)))
receive timeoutVar connection handlers = do
  Connection.receive timeoutVar connection >>= \case
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
