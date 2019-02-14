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
  -> Proto.RpbDelReq
  -> IO (Either Error ())
delete handle request =
  exchange
    handle
    (ReqRpbDel request)
    (\case
      RespRpbDel{} -> Just ()
      _ -> Nothing)

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either Error ())
deleteIndex handle name =
  exchange
    handle
    (ReqRpbYokozunaIndexDelete request)
    (\case
      RespRpbDel{} -> Just ()
      _ -> Nothing)
  where
    request :: Proto.RpbYokozunaIndexDeleteReq
    request =
      defMessage
        & Proto.name .~ name

get ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Either Error Proto.RpbGetResp)
get handle request =
  exchange
    handle
    (ReqRpbGet request)
    (\case
      RespRpbGet response -> Just response
      _ -> Nothing)

getBucket ::
     Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either Error Proto.RpbBucketProps)
getBucket handle request =
  exchange
    handle
    (ReqRpbGetBucket request)
    (\case
      RespRpbGetBucket response -> Just (response ^. Proto.props)
      _ -> Nothing)

getBucketType ::
     Handle -- ^
  -> ByteString -- ^ Bucket type
  -> IO (Either Error Proto.RpbBucketProps)
getBucketType handle bucketType =
  exchange
    handle
    (ReqRpbGetBucketType request)
    (\case
      RespRpbGetBucket response -> Just (response ^. Proto.props)
      _ -> Nothing)

  where
    request :: Proto.RpbGetBucketTypeReq
    request =
      defMessage
        & Proto.type' .~ bucketType

getCrdt ::
     Handle
  -> Proto.DtFetchReq
  -> IO (Either Error Proto.DtFetchResp)
getCrdt handle request =
  exchange
    handle
    (ReqDtFetch request)
    (\case
      RespDtFetch response -> Just response
      _ -> Nothing)

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either Error [Proto.RpbYokozunaIndex])
getIndex handle name =
  exchange
    handle
    (ReqRpbYokozunaIndexGet request)
    (\case
      RespRpbYokozunaIndexGet response -> Just (response ^. Proto.index)
      _ -> Nothing)
  where
    request :: Proto.RpbYokozunaIndexGetReq
    request =
      defMessage
        & Proto.maybe'name .~ name

getSchema ::
     Handle
  -> ByteString
  -> IO (Either Error Proto.RpbYokozunaSchema)
getSchema handle name =
  exchange
    handle
    (ReqRpbYokozunaSchemaGet request)
    (\case
      RespRpbYokozunaSchemaGet response -> Just (response ^. Proto.schema)
      _ -> Nothing)

  where
    request :: Proto.RpbYokozunaSchemaGetReq
    request =
      defMessage
        & Proto.name .~ name

getServerInfo ::
     Handle
  -> IO (Either Error Proto.RpbGetServerInfoResp)
getServerInfo handle =
  exchange
    handle
    (ReqRpbGetServerInfo defMessage)
    (\case
      RespRpbGetServerInfo response -> Just response
      _ -> Nothing)

listBuckets ::
     Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either Error r)
listBuckets handle request =
  stream
    handle
    (ReqRpbListBuckets request)
    (\case
      RespRpbListBuckets response -> Just response
      _ -> Nothing)
    (view Proto.done)

listKeys ::
     Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either Error r)
listKeys handle request =
  stream
    handle
    (ReqRpbListKeys request)
    (\case
      RespRpbListKeys response -> Just response
      _ -> Nothing)
    (view Proto.done)

mapReduce ::
     Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either Error r)
mapReduce handle request =
  stream
    handle
    (ReqRpbMapRed request)
    (\case
      RespRpbMapRed response -> Just response
      _ -> Nothing)
    (view Proto.done)

ping ::
     Handle
  -> IO (Either Error ())
ping handle =
  exchange
    handle
    (ReqRpbPing defMessage)
    (\case
      RespRpbPing _ -> Just ()
      _ -> Nothing)

put ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either Error Proto.RpbPutResp)
put handle request =
  exchange
    handle
    (ReqRpbPut request)
    (\case
      RespRpbPut response -> Just response
      _ -> Nothing)

putIndex ::
     Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either Error ())
putIndex handle request =
  exchange
    handle
    (ReqRpbYokozunaIndexPut request)
    (\case
      RespRpbPut{} -> Just ()
      _ -> Nothing)

putSchema ::
     Handle
  -> Proto.RpbYokozunaSchema
  -> IO (Either Error ())
putSchema handle schema =
  exchange
    handle
    (ReqRpbYokozunaSchemaPut request)
    (\case
      RespRpbPut{} -> Just ()
      _ -> Nothing)

  where
    request :: Proto.RpbYokozunaSchemaPutReq
    request =
      defMessage
        & Proto.schema .~ schema

resetBucket ::
     Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either Error ())
resetBucket handle request =
  exchange
    handle
    (ReqRpbResetBucket request)
    (\case
      RespRpbResetBucket _ -> Just ()
      _ -> Nothing)

setBucket ::
     Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either Error ())
setBucket handle request =
  exchange
    handle
    (ReqRpbSetBucket request)
    (\case
      RespRpbSetBucket{} -> Just ()
      _ -> Nothing)

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either Error ())
setBucketType handle request =
  exchange
    handle
    (ReqRpbSetBucketType request)
    (\case
      RespRpbSetBucket{} -> Just ()
      _ -> Nothing)

search ::
     Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either Error Proto.RpbSearchQueryResp)
search handle request =
  exchange
    handle
    (ReqRpbSearchQuery request)
    (\case
      RespRpbSearchQuery response -> Just response
      _ -> Nothing)

secondaryIndex ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either Error r)
secondaryIndex handle request =
  stream
    handle
    (ReqRpbIndex request)
    (\case
      RespRpbIndex response -> Just response
      _ -> Nothing)
    (view Proto.done)

updateCrdt ::
     Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either Error Proto.DtUpdateResp)
updateCrdt handle request =
  exchange
    handle
    (ReqDtUpdate request)
    (\case
      RespDtUpdate response -> Just response
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

    Right (RespRpbError response) ->
      pure (Left (ErrorRiak (response ^. Proto.errmsg)))

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
      RespRpbError response ->
        pure (Right (Left (response ^. Proto.errmsg)))

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
