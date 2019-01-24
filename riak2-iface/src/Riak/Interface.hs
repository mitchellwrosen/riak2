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
import Riak.Message             (Message)
import Riak.Proto
import Riak.Request             (Request)
import Riak.Response            (Response)

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto.Lens          as L
import qualified Riak.Request             as Request
import qualified Riak.Response            as Response

import Control.Exception      (throwIO)
import Control.Foldl          (FoldM(..))
import Control.Lens           (view, (^.))
import Data.ProtoLens.Message (defMessage)


data Result a
  = RiakClosedConnection
  | Failure ErrorResponse
  | Success a
  deriving stock (Eq, Functor, Show)

exchange ::
     forall a b.
     (Request a, Response b)
  => Interface
  -> a
  -> IO (Result b)
exchange iface request =
  Interface.exchange iface (Request.toMessage request) >>=
    toResult

stream ::
     forall a b r.
     (Request a, Response b)
  => Interface
  -> a -- ^ Request
  -> (b -> Bool) -- ^ Done?
  -> FoldM IO b r -- ^ Fold responses
  -> IO (Result r)
stream iface request done (FoldM step initial extract) =
  Interface.stream
    iface
    (Request.toMessage request)
    callback

  where
    callback :: IO (Maybe Message) -> IO (Result r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= toResult >>= \case
            RiakClosedConnection ->
              pure RiakClosedConnection

            Failure err ->
              pure (Failure err)

            Success message -> do
              value' <-
                step value message

              if done message
                then
                  Success <$> extract value'
                else
                  loop value'

toResult :: Response a => Maybe Message -> IO (Result a)
toResult = \case
  Nothing ->
    pure RiakClosedConnection

  Just message ->
    case Response.parse message of
      Left err ->
        throwIO err

      Right (Left response) ->
        pure (Failure response)

      Right (Right response) ->
        pure (Success response)


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
delete =
  exchange

getBucketProperties
  :: Interface -- ^
  -> GetBucketPropertiesRequest -- ^
  -> IO (Result GetBucketPropertiesResponse)
getBucketProperties =
  exchange

getBucketTypeProperties
  :: Interface -- ^
  -> GetBucketTypePropertiesRequest -- ^
  -> IO (Result GetBucketPropertiesResponse)
getBucketTypeProperties =
  exchange

getCrdt
  :: Interface -- ^
  -> GetCrdtRequest -- ^
  -> IO (Result GetCrdtResponse)
getCrdt
  = exchange

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
get =
  exchange

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
  exchange conn GetServerInfoRequest

ping
  :: Interface -- ^
  -> IO (Result PingResponse)
ping conn = do
  exchange conn (defMessage :: PingRequest)

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
put =
  exchange

-- putSchema
--   :: Interface -- ^
--   -> RpbYokozunaSchemaPutReq -- ^
--   -> IO (Result RpbEmptyPutResp)
-- putSchema =
--   exchange

resetBucketProperties
  :: Interface -- ^
  -> ResetBucketPropertiesRequest -- ^
  -> IO (Result ResetBucketPropertiesResponse)
resetBucketProperties =
  exchange

index
  :: Interface -- ^
  -> IndexRequest -- ^
  -> FoldM IO IndexResponse r -- ^
  -> IO (Result r)
index conn request =
  stream conn request (view L.done)

mapReduce
  :: Interface -- ^
  -> MapReduceRequest -- ^
  -> FoldM IO MapReduceResponse r -- ^
  -> IO (Result r)
mapReduce conn request =
  stream conn request (view L.done)

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
setBucketProperties =
  exchange

setBucketTypeProperties
  :: Interface -- ^
  -> SetBucketTypePropertiesRequest -- ^
  -> IO (Result SetBucketPropertiesResponse)
setBucketTypeProperties =
  exchange

streamBuckets
  :: Interface -- ^
  -> StreamBucketsRequest -- ^
  -> FoldM IO StreamBucketsResponse r -- ^
  -> IO (Result r)
streamBuckets conn request =
  stream conn request (view L.done)

streamKeys
  :: Interface -- ^
  -> StreamKeysRequest -- ^
  -> FoldM IO StreamKeysResponse r -- ^
  -> IO (Result r)
streamKeys conn request =
  stream conn request (view L.done)

updateCrdt
  :: Interface -- ^
  -> UpdateCrdtRequest -- ^
  -> IO (Result UpdateCrdtResponse)
updateCrdt =
  exchange
