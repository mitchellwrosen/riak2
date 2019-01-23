module Riak.Interface
  ( Interface
  , Interface.connect
  , Interface.disconnect
  , deleteIndex
  , delete
  , getBucketProps
  , getBucketTypeProps
  , getCrdt
  , getIndex
  , get
  , getSchema
  , getServerInfo
  , index
  , mapReduce
  , ping
  , putIndex
  , put
  , putSchema
  , resetBucketProps
  , search
  , setBucketProps
  , setBucketTypeProps
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

import Control.Exception (throwIO)
import Control.Foldl     (FoldM(..))
import Control.Lens      (view, (^.))


data Result a
  = RiakClosedConnection
  | Failure RpbErrorResp
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

deleteIndex
  :: Interface -- ^
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (Result RpbDelResp)
deleteIndex =
  exchange

delete
  :: Interface -- ^
  -> RpbDelReq -- ^
  -> IO (Result RpbDelResp)
delete =
  exchange

getBucketProps
  :: Interface -- ^
  -> RpbGetBucketReq -- ^
  -> IO (Result RpbGetBucketResp)
getBucketProps =
  exchange

getBucketTypeProps
  :: Interface -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (Result RpbGetBucketResp)
getBucketTypeProps =
  exchange

getCrdt
  :: Interface -- ^
  -> DtFetchReq -- ^
  -> IO (Result DtFetchResp)
getCrdt
  = exchange

getIndex
  :: Interface -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (Result RpbYokozunaIndexGetResp)
getIndex =
  exchange

get
  :: Interface -- ^
  -> RpbGetReq -- ^
  -> IO (Result RpbGetResp)
get =
  exchange

getSchema
  :: Interface -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (Result RpbYokozunaSchemaGetResp)
getSchema =
  exchange

getServerInfo
  :: Interface -- ^
  -> IO (Result RpbGetServerInfoResp)
getServerInfo conn =
  exchange conn RpbGetServerInfoReq

ping
  :: Interface -- ^
  -> IO (Result RpbPingResp)
ping conn = do
  exchange conn RpbPingReq

putIndex
  :: Interface -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (Result RpbEmptyPutResp)
putIndex =
  exchange

put
  :: Interface -- ^
  -> RpbPutReq -- ^
  -> IO (Result RpbPutResp)
put =
  exchange

putSchema
  :: Interface -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (Result RpbEmptyPutResp)
putSchema =
  exchange

resetBucketProps
  :: Interface -- ^
  -> RpbResetBucketReq -- ^
  -> IO (Result RpbResetBucketResp)
resetBucketProps =
  exchange

index
  :: Interface -- ^
  -> RpbIndexReq -- ^
  -> FoldM IO RpbIndexResp r -- ^
  -> IO (Result r)
index conn request =
  stream conn request (view L.done)

mapReduce
  :: Interface -- ^
  -> RpbMapRedReq -- ^
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Result r)
mapReduce conn request =
  stream conn request (view L.done)

search
  :: Interface -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (Result RpbSearchQueryResp)
search =
  exchange

setBucketProps
  :: Interface -- ^
  -> RpbSetBucketReq -- ^
  -> IO (Result RpbSetBucketResp)
setBucketProps =
  exchange

setBucketTypeProps
  :: Interface -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (Result RpbSetBucketTypeResp)
setBucketTypeProps =
  exchange

streamBuckets
  :: Interface -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (Result r)
streamBuckets conn request =
  stream conn request (view L.done)

streamKeys
  :: Interface -- ^
  -> RpbListKeysReq -- ^
  -> FoldM IO RpbListKeysResp r -- ^
  -> IO (Result r)
streamKeys conn request =
  stream conn request (view L.done)

updateCrdt
  :: Interface -- ^
  -> DtUpdateReq -- ^
  -> IO (Result DtUpdateResp)
updateCrdt =
  exchange
