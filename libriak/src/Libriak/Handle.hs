module Libriak.Handle
  ( Handle
  , Config(..)
  , withHandle
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
  , Error(..)
  , UnexpectedResponse(..)
  ) where

import Libriak.Request       (Request(..))
import Libriak.Response      (Response(..))
import Riak.Handle.Signature (Config(..), Handle, withHandle)

import qualified Libriak.Proto         as Proto
import qualified Libriak.Proto.Lens    as L
import qualified Riak.Handle.Signature as Handle

import Control.Exception      (Exception, throwIO)
import Control.Foldl          (FoldM(..))
import Control.Lens           (view, (.~), (^.))
import Data.Bifunctor         (first)
import Data.ByteString        (ByteString)
import Data.Function          ((&))
import Data.ProtoLens.Message (defMessage)

import qualified Control.Foldl as Foldl


data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

data Error
  = ErrorRiak ByteString -- ^ Riak returned an error response.
  | ErrorHandle Handle.Error -- ^ A handle error occurred.
  deriving stock (Eq, Show)

delete ::
     Handle
  -> Proto.DeleteRequest
  -> IO (Either Error ())
delete handle request =
  exchange
    handle
    (RequestDelete request)
    (\case
      ResponseDelete{} -> Just ()
      _ -> Nothing)

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either Error ())
deleteIndex handle name =
  exchange
    handle
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
  -> IO (Either Error Proto.GetResponse)
get handle request =
  exchange
    handle
    (RequestGet request)
    (\case
      ResponseGet response -> Just response
      _ -> Nothing)

getBucket ::
     Handle
  -> Proto.GetBucketRequest
  -> IO (Either Error Proto.BucketProperties)
getBucket handle request =
  exchange
    handle
    (RequestGetBucket request)
    (\case
      ResponseGetBucket response -> Just (response ^. L.props)
      _ -> Nothing)

getBucketType ::
     Handle
  -> ByteString
  -> IO (Either Error Proto.BucketProperties)
getBucketType handle bucketType =
  exchange
    handle
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
  -> IO (Either Error Proto.GetCrdtResponse)
getCrdt handle request =
  exchange
    handle
    (RequestGetCrdt request)
    (\case
      ResponseGetCrdt response -> Just response
      _ -> Nothing)

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either Error [Proto.Index])
getIndex handle name =
  exchange
    handle
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
  -> IO (Either Error Proto.Schema)
getSchema handle name =
  exchange
    handle
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
  -> IO (Either Error Proto.GetServerInfoResponse)
getServerInfo handle =
  exchange
    handle
    (RequestGetServerInfo defMessage)
    (\case
      ResponseGetServerInfo response -> Just response
      _ -> Nothing)

listBuckets ::
     Handle
  -> Proto.ListBucketsRequest
  -> FoldM IO Proto.ListBucketsResponse r
  -> IO (Either Error r)
listBuckets handle request =
  stream
    handle
    (RequestListBuckets request)
    (\case
      ResponseListBuckets response -> Just response
      _ -> Nothing)
    (view L.done)

listKeys ::
     Handle
  -> Proto.ListKeysRequest
  -> FoldM IO Proto.ListKeysResponse r
  -> IO (Either Error r)
listKeys handle request =
  stream
    handle
    (RequestListKeys request)
    (\case
      ResponseListKeys response -> Just response
      _ -> Nothing)
    (view L.done)

mapReduce ::
     Handle
  -> Proto.MapReduceRequest
  -> FoldM IO Proto.MapReduceResponse r
  -> IO (Either Error r)
mapReduce handle request =
  stream
    handle
    (RequestMapReduce request)
    (\case
      ResponseMapReduce response -> Just response
      _ -> Nothing)
    (view L.done)

ping ::
     Handle
  -> IO (Either Error ())
ping handle =
  exchange
    handle
    (RequestPing defMessage)
    (\case
      ResponsePing _ -> Just ()
      _ -> Nothing)

put ::
     Handle
  -> Proto.PutRequest
  -> IO (Either Error Proto.PutResponse)
put handle request =
  exchange
    handle
    (RequestPut request)
    (\case
      ResponsePut response -> Just response
      _ -> Nothing)

putIndex ::
     Handle
  -> Proto.PutIndexRequest
  -> IO (Either Error ())
putIndex handle request =
  exchange
    handle
    (RequestPutIndex request)
    (\case
      ResponsePut{} -> Just ()
      _ -> Nothing)

putSchema ::
     Handle
  -> Proto.Schema
  -> IO (Either Error ())
putSchema handle schema =
  exchange
    handle
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
  -> IO (Either Error ())
resetBucket handle request =
  exchange
    handle
    (RequestResetBucket request)
    (\case
      ResponseResetBucket _ -> Just ()
      _ -> Nothing)

setBucket ::
     Handle
  -> Proto.SetBucketRequest
  -> IO (Either Error ())
setBucket handle request =
  exchange
    handle
    (RequestSetBucket request)
    (\case
      ResponseSetBucket{} -> Just ()
      _ -> Nothing)

setBucketType ::
     Handle
  -> Proto.SetBucketTypeRequest
  -> IO (Either Error ())
setBucketType handle request =
  exchange
    handle
    (RequestSetBucketType request)
    (\case
      ResponseSetBucket{} -> Just ()
      _ -> Nothing)

search ::
     Handle
  -> Proto.SearchRequest
  -> IO (Either Error Proto.SearchResponse)
search handle request =
  exchange
    handle
    (RequestSearch request)
    (\case
      ResponseSearch response -> Just response
      _ -> Nothing)

secondaryIndex ::
     Handle
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either Error r)
secondaryIndex handle request =
  stream
    handle
    (RequestSecondaryIndex request)
    (\case
      ResponseSecondaryIndex response -> Just response
      _ -> Nothing)
    (view L.done)

updateCrdt ::
     Handle -- ^
  -> Proto.UpdateCrdtRequest -- ^
  -> IO (Either Error Proto.UpdateCrdtResponse)
updateCrdt handle request =
  exchange
    handle
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)

exchange ::
     Handle
  -> Request
  -> (Response -> Maybe a)
  -> IO (Either Error a)
exchange handle request f =
  Handle.exchange handle request >>= \case
    Left err ->
      pure (Left (ErrorHandle err))

    Right (ResponseError response) ->
      pure (Left (ErrorRiak (response ^. L.errmsg)))

    Right response ->
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
  -> IO (Either Error r)
stream handle request f done (FoldM step (initial :: IO x) extract) = do
  initial' <- initial

  fromResult <$>
    Handle.stream
      handle
      request
      initial'
      step'

  where
    step' ::
         x
      -> Response
      -> IO (Either x (Either ByteString r))
    step' value = \case
      ResponseError response ->
        pure (Right (Left (response ^. L.errmsg)))

      response ->
        case f response of
          Nothing ->
            throwIO (UnexpectedResponse request response)

          Just response' -> do
            newValue :: x <-
              step value response'

            if done response'
              then
                Right . Right <$> extract newValue
              else
                pure (Left newValue)

    fromResult ::
         Either Handle.Error (Either ByteString r)
      -> Either Error r
    fromResult = \case
      Left err ->
        Left (ErrorHandle err)

      Right (Left err) ->
        Left (ErrorRiak err)

      Right (Right result') ->
        Right result'
