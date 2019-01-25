-- TODO Move exchange/stream to Riak.Internal.Client, inline everything else

module Riak.Interface
  ( Interface
  , Interface.connect
  , Interface.disconnect
  -- , deleteIndex
  , delete
  , getBucketProperties
  , getBucketTypeProperties
  , getCrdt
  -- , getIndex
  , get
  -- , getSchema
  , getServerInfo
  , index
  , mapReduce
  , ping
  -- , putIndex
  , put
  -- , putSchema
  , resetBucketProperties
  -- , search
  , setBucketProperties
  , setBucketTypeProperties
  , streamBuckets
  , streamKeys
  , updateCrdt
  , Result(..)
  ) where

import Riak.Interface.Signature (Interface)
import Riak.Proto
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto.Lens          as L
import qualified Riak.Request             as Request
import qualified Riak.Response            as Response

import Control.Exception      (Exception, throwIO)
import Control.Foldl          (FoldM(..))
import Control.Lens           (view, (^.))
import Data.ProtoLens.Message (defMessage)


data Result a
  = Success a
  | Failure ErrorResponse
  | ConnectionClosed
  deriving stock (Functor, Show)

data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

exchange ::
     Interface
  -> Request
  -> (Response -> Maybe a)
  -> IO (Result a)
exchange conn request f =
  Interface.exchange conn request >>= \case
    Nothing ->
      pure ConnectionClosed

    Just (ResponseError response) ->
      pure (Failure response)

    Just response ->
      case f response of
        Nothing ->
          throwIO (UnexpectedResponse request response)

        Just response' ->
          pure (Success response')

stream ::
     forall a r.
     Interface
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Result r)
stream iface request f done (FoldM step initial extract) =
  Interface.stream
    iface
    request
    callback

  where
    callback :: IO (Maybe Response) -> IO (Result r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= \case
            Nothing ->
              pure ConnectionClosed

            Just (ResponseError response) ->
              pure (Failure response)

            Just response ->
              case f response of
                Nothing ->
                  throwIO (UnexpectedResponse request response)

                Just response' -> do
                  value' <-
                    step value response'

                  if done response'
                    then
                      Success <$> extract value'
                    else
                      loop value'


--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

-- deleteIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexDeleteReq -- ^
--   -> IO (Result RpbDelResp)
-- deleteIndex =
--   exchange

delete
  :: Interface -- ^
  -> DeleteRequest -- ^
  -> IO (Result DeleteResponse)
delete conn request =
  exchange
    conn
    (RequestDelete request)
    (\case
      ResponseDelete response -> Just response
      _ -> Nothing)

getBucketProperties
  :: Interface -- ^
  -> GetBucketPropertiesRequest -- ^
  -> IO (Result GetBucketPropertiesResponse)
getBucketProperties conn request =
  exchange
    conn
    (RequestGetBucketProperties request)
    (\case
      ResponseGetBucketProperties response -> Just response
      _ -> Nothing)

getBucketTypeProperties
  :: Interface -- ^
  -> GetBucketTypePropertiesRequest -- ^
  -> IO (Result GetBucketPropertiesResponse)
getBucketTypeProperties conn request =
  exchange
    conn
    (RequestGetBucketTypeProperties request)
    (\case
      ResponseGetBucketProperties response -> Just response
      _ -> Nothing)

getCrdt
  :: Interface -- ^
  -> GetCrdtRequest -- ^
  -> IO (Result GetCrdtResponse)
getCrdt conn request =
  exchange
    conn
    (RequestGetCrdt request)
    (\case
      ResponseGetCrdt response -> Just response
      _ -> Nothing)

-- getIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexGetReq -- ^
--   -> IO (Result RpbYokozunaIndexGetResp)
-- getIndex =
--   exchange

get
  :: Interface -- ^
  -> GetRequest -- ^
  -> IO (Result GetResponse)
get conn request =
  exchange
    conn
    (RequestGet request)
    (\case
      ResponseGet response -> Just response
      _ -> Nothing)

-- getSchema
--   :: Interface -- ^
--   -> RpbYokozunaSchemaGetReq -- ^
--   -> IO (Result RpbYokozunaSchemaGetResp)
-- getSchema =
--   exchange

getServerInfo
  :: Interface -- ^
  -> IO (Result GetServerInfoResponse)
getServerInfo conn =
  exchange
    conn
    (RequestGetServerInfo defMessage)
    (\case
      ResponseGetServerInfo response -> Just response
      _ -> Nothing)

ping
  :: Interface -- ^
  -> IO (Result ())
ping conn = do
  exchange
    conn
    (RequestPing defMessage)
    (\case
      ResponsePing _ -> Just ()
      _ -> Nothing)

-- putIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexPutReq -- ^
--   -> IO (Result RpbEmptyPutResp)
-- putIndex =
--   exchange

put
  :: Interface -- ^
  -> PutRequest -- ^
  -> IO (Result PutResponse)
put conn request =
  exchange
    conn
    (RequestPut request)
    (\case
      ResponsePut response -> Just response
      _ -> Nothing)

-- putSchema
--   :: Interface -- ^
--   -> RpbYokozunaSchemaPutReq -- ^
--   -> IO (Result RpbEmptyPutResp)
-- putSchema =
--   exchange

resetBucketProperties
  :: Interface -- ^
  -> ResetBucketPropertiesRequest -- ^
  -> IO (Result ())
resetBucketProperties conn request =
  exchange
    conn
    (RequestResetBucketProperties request)
    (\case
      ResponseResetBucketProperties _ -> Just ()
      _ -> Nothing)

index
  :: Interface -- ^
  -> IndexRequest -- ^
  -> FoldM IO IndexResponse r -- ^
  -> IO (Result r)
index conn request =
  stream
    conn
    (RequestIndex request)
    (\case
      ResponseIndex response -> Just response
      _ -> Nothing)
    (view L.done)

mapReduce
  :: Interface -- ^
  -> MapReduceRequest -- ^
  -> FoldM IO MapReduceResponse r -- ^
  -> IO (Result r)
mapReduce conn request =
  stream
    conn
    (RequestMapReduce request)
    (\case
      ResponseMapReduce response -> Just response
      _ -> Nothing)
    (view L.done)

-- search
--   :: Interface -- ^
--   -> RpbSearchQueryReq -- ^
--   -> IO (Result RpbSearchQueryResp)
-- search =
--   exchange

setBucketProperties
  :: Interface -- ^
  -> SetBucketPropertiesRequest -- ^
  -> IO (Result SetBucketPropertiesResponse)
setBucketProperties conn request =
  exchange
    conn
    (RequestSetBucketProperties request)
    (\case
      ResponseSetBucketProperties response -> Just response
      _ -> Nothing)

setBucketTypeProperties
  :: Interface -- ^
  -> SetBucketTypePropertiesRequest -- ^
  -> IO (Result SetBucketPropertiesResponse)
setBucketTypeProperties conn request =
  exchange
    conn
    (RequestSetBucketTypeProperties request)
    (\case
      ResponseSetBucketProperties response -> Just response
      _ -> Nothing)

streamBuckets
  :: Interface -- ^
  -> StreamBucketsRequest -- ^
  -> FoldM IO StreamBucketsResponse r -- ^
  -> IO (Result r)
streamBuckets conn request =
  stream
    conn
    (RequestStreamBuckets request)
    (\case
      ResponseStreamBuckets response -> Just response
      _ -> Nothing)
    (view L.done)

streamKeys
  :: Interface -- ^
  -> StreamKeysRequest -- ^
  -> FoldM IO StreamKeysResponse r -- ^
  -> IO (Result r)
streamKeys conn request =
  stream
    conn
    (RequestStreamKeys request)
    (\case
      ResponseStreamKeys response -> Just response
      _ -> Nothing)
    (view L.done)

updateCrdt
  :: Interface -- ^
  -> UpdateCrdtRequest -- ^
  -> IO (Result UpdateCrdtResponse)
updateCrdt conn request =
  exchange
    conn
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)
