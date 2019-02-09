module Riak.Handle
  ( Handle
  , UnexpectedResponse(..)
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
  , secondaryIndex
  , setBucket
  , setBucketType
  , updateCrdt
  ) where

import Riak.Handle.Signature (Handle)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Handle.Signature as Handle
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Control.Exception      (Exception, throwIO)
import Control.Foldl          (FoldM(..))
import Control.Lens           (view, (.~), (^.))
import Data.ByteString        (ByteString)
import Data.Function          ((&))
import Data.ProtoLens.Message (defMessage)


data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

delete ::
     Handle
  -> Proto.DeleteRequest
  -> IO (Either ByteString ())
delete iface request =
  exchange
    iface
    (RequestDelete request)
    (\case
      ResponseDelete{} -> Just ()
      _ -> Nothing)

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either ByteString ())
deleteIndex iface name =
  exchange
    iface
    (RequestDeleteIndex request)
    (\case
      ResponseDelete{} -> Just ()
      _ -> Nothing)
  where
    request :: Proto.DeleteIndexRequest
    request =
      defMessage
        & L.name .~ name

get ::
     Handle
  -> Proto.GetRequest
  -> IO (Either ByteString Proto.GetResponse)
get iface request =
  exchange
    iface
    (RequestGet request)
    (\case
      ResponseGet response -> Just response
      _ -> Nothing)

getBucket ::
     Handle
  -> Proto.GetBucketRequest
  -> IO (Either ByteString Proto.BucketProperties)
getBucket iface request =
  exchange
    iface
    (RequestGetBucket request)
    (\case
      ResponseGetBucket response -> Just (response ^. L.props)
      _ -> Nothing)

getBucketType ::
     Handle
  -> ByteString
  -> IO (Either ByteString Proto.BucketProperties)
getBucketType iface bucketType =
  exchange
    iface
    (RequestGetBucketType request)
    (\case
      ResponseGetBucket response -> Just (response ^. L.props)
      _ -> Nothing)

  where
    request :: Proto.GetBucketTypeRequest
    request =
      defMessage
        & L.bucketType .~ bucketType

getCrdt ::
     Handle
  -> Proto.GetCrdtRequest
  -> IO (Either ByteString Proto.GetCrdtResponse)
getCrdt iface request =
  exchange
    iface
    (RequestGetCrdt request)
    (\case
      ResponseGetCrdt response -> Just response
      _ -> Nothing)

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either ByteString [Proto.Index])
getIndex iface name =
  exchange
    iface
    (RequestGetIndex request)
    (\case
      ResponseGetIndex response -> Just (response ^. L.index)
      _ -> Nothing)
  where
    request :: Proto.GetIndexRequest
    request =
      defMessage
        & L.maybe'name .~ name

getSchema ::
     Handle
  -> ByteString
  -> IO (Either ByteString Proto.Schema)
getSchema iface name =
  exchange
    iface
    (RequestGetSchema request)
    (\case
      ResponseGetSchema response -> Just (response ^. L.schema)
      _ -> Nothing)

  where
    request :: Proto.GetSchemaRequest
    request =
      defMessage
        & L.name .~ name

getServerInfo ::
     Handle
  -> IO (Either ByteString Proto.GetServerInfoResponse)
getServerInfo iface =
  exchange
    iface
    (RequestGetServerInfo defMessage)
    (\case
      ResponseGetServerInfo response -> Just response
      _ -> Nothing)

listBuckets ::
     Handle
  -> Proto.ListBucketsRequest
  -> FoldM IO Proto.ListBucketsResponse r
  -> IO (Either ByteString r)
listBuckets iface request =
  stream
    iface
    (RequestListBuckets request)
    (\case
      ResponseListBuckets response -> Just response
      _ -> Nothing)
    (view L.done)

listKeys ::
     Handle
  -> Proto.ListKeysRequest
  -> FoldM IO Proto.ListKeysResponse r
  -> IO (Either ByteString r)
listKeys iface request =
  stream
    iface
    (RequestListKeys request)
    (\case
      ResponseListKeys response -> Just response
      _ -> Nothing)
    (view L.done)

mapReduce ::
     Handle
  -> Proto.MapReduceRequest
  -> FoldM IO Proto.MapReduceResponse r
  -> IO (Either ByteString r)
mapReduce iface request =
  stream
    iface
    (RequestMapReduce request)
    (\case
      ResponseMapReduce response -> Just response
      _ -> Nothing)
    (view L.done)

ping ::
     Handle
  -> IO (Either ByteString ())
ping iface =
  exchange
    iface
    (RequestPing defMessage)
    (\case
      ResponsePing _ -> Just ()
      _ -> Nothing)

put ::
     Handle
  -> Proto.PutRequest
  -> IO (Either ByteString Proto.PutResponse)
put iface request =
  exchange
    iface
    (RequestPut request)
    (\case
      ResponsePut response -> Just response
      _ -> Nothing)

putIndex ::
     Handle
  -> Proto.PutIndexRequest
  -> IO (Either ByteString ())
putIndex iface request =
  exchange
    iface
    (RequestPutIndex request)
    (\case
      ResponsePut{} -> Just ()
      _ -> Nothing)

putSchema ::
     Handle
  -> Proto.Schema
  -> IO (Either ByteString ())
putSchema iface schema =
  exchange
    iface
    (RequestPutSchema request)
    (\case
      ResponsePut{} -> Just ()
      _ -> Nothing)

  where
    request :: Proto.PutSchemaRequest
    request =
      defMessage
        & L.schema .~ schema

resetBucket ::
     Handle
  -> Proto.ResetBucketRequest
  -> IO (Either ByteString ())
resetBucket iface request =
  exchange
    iface
    (RequestResetBucket request)
    (\case
      ResponseResetBucket _ -> Just ()
      _ -> Nothing)

setBucket ::
     Handle
  -> Proto.SetBucketRequest
  -> IO (Either ByteString ())
setBucket iface request =
  exchange
    iface
    (RequestSetBucket request)
    (\case
      ResponseSetBucket{} -> Just ()
      _ -> Nothing)

setBucketType ::
     Handle
  -> Proto.SetBucketTypeRequest
  -> IO (Either ByteString ())
setBucketType iface request =
  exchange
    iface
    (RequestSetBucketType request)
    (\case
      ResponseSetBucket{} -> Just ()
      _ -> Nothing)

secondaryIndex ::
     Handle
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either ByteString r)
secondaryIndex iface request =
  stream
    iface
    (RequestSecondaryIndex request)
    (\case
      ResponseSecondaryIndex response -> Just response
      _ -> Nothing)
    (view L.done)

updateCrdt ::
     Handle -- ^
  -> Proto.UpdateCrdtRequest -- ^
  -> IO (Either ByteString Proto.UpdateCrdtResponse)
updateCrdt iface request =
  exchange
    iface
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)

exchange ::
     Handle
  -> Request
  -> (Response -> Maybe a)
  -> IO (Either ByteString a)
exchange iface request f =
  Handle.exchange iface request >>= \case
    ResponseError response ->
      pure (Left (response ^. L.errmsg))

    response ->
      case f response of
        Nothing ->
          throwIO (UnexpectedResponse request response)

        Just response' ->
          pure (Right response')

stream ::
     forall a r.
     Handle
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Either ByteString r)
stream iface request f done (FoldM step initial extract) =
  Handle.stream iface request callback

  where
    callback :: IO Response -> IO (Either ByteString r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= \case
            ResponseError response ->
              pure (Left (response ^. L.errmsg))

            response ->
              case f response of
                Nothing ->
                  throwIO (UnexpectedResponse request response)

                Just response' -> do
                  value' <-
                    step value response'

                  if done response'
                    then
                      Right <$> extract value'
                    else
                      loop value'
