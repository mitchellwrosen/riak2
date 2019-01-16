module Riak.Interface
  ( Interface
  , Interface.connect
  , Interface.disconnect
  , deleteIndex
  , deleteObject
  , getBucketProps
  , getBucketTypeProps
  , getCrdt
  , getIndex
  , getObject
  , getSchema
  , getServerInfo
  , index
  , mapReduce
  , ping
  , putIndex
  , putObject
  , putSchema
  , resetBucketProps
  , search
  , setBucketProps
  , setBucketTypeProps
  , streamBuckets
  , streamKeys
  , updateCrdt
  , RecvResult(..)
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


data RecvResult a
  = RiakClosedConnection
  | Failure RpbErrorResp
  | Success a
  deriving stock (Eq, Functor, Show)

exchange ::
     forall a b.
     (Request a, Response b)
  => Interface
  -> a
  -> IO (RecvResult b)
exchange iface request =
  Interface.exchange iface (Request.toMessage request) >>=
    toRecvResult

stream ::
     forall a b r.
     (Request a, Response b)
  => Interface
  -> a -- ^ Request
  -> (b -> Bool) -- ^ Done?
  -> FoldM IO b r -- ^ Fold responses
  -> IO (RecvResult r)
stream iface request done (FoldM step initial extract) =
  Interface.stream
    iface
    (Request.toMessage request)
    callback

  where
    callback :: IO (Maybe Message) -> IO (RecvResult r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= toRecvResult >>= \case
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

toRecvResult :: Response a => Maybe Message -> IO (RecvResult a)
toRecvResult = \case
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
  -> IO (RecvResult RpbDelResp)
deleteIndex =
  exchange

deleteObject
  :: Interface -- ^
  -> RpbDelReq -- ^
  -> IO (RecvResult RpbDelResp)
deleteObject =
  exchange

getBucketProps
  :: Interface -- ^
  -> RpbGetBucketReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketProps =
  exchange

getBucketTypeProps
  :: Interface -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketTypeProps =
  exchange

getCrdt
  :: Interface -- ^
  -> DtFetchReq -- ^
  -> IO (RecvResult DtFetchResp)
getCrdt
  = exchange

getIndex
  :: Interface -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (RecvResult RpbYokozunaIndexGetResp)
getIndex =
  exchange

getObject
  :: Interface -- ^
  -> RpbGetReq -- ^
  -> IO (RecvResult RpbGetResp)
getObject =
  exchange

getSchema
  :: Interface -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (RecvResult RpbYokozunaSchemaGetResp)
getSchema =
  exchange

getServerInfo
  :: Interface -- ^
  -> IO (RecvResult RpbGetServerInfoResp)
getServerInfo conn =
  exchange conn RpbGetServerInfoReq

ping
  :: Interface -- ^
  -> IO (RecvResult RpbPingResp)
ping conn = do
  exchange conn RpbPingReq

putIndex
  :: Interface -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putIndex =
  exchange

putObject
  :: Interface -- ^
  -> RpbPutReq -- ^
  -> IO (RecvResult RpbPutResp)
putObject =
  exchange

putSchema
  :: Interface -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putSchema =
  exchange

resetBucketProps
  :: Interface -- ^
  -> RpbResetBucketReq -- ^
  -> IO (RecvResult RpbResetBucketResp)
resetBucketProps =
  exchange

index
  :: Interface -- ^
  -> RpbIndexReq -- ^
  -> FoldM IO RpbIndexResp r -- ^
  -> IO (RecvResult r)
index conn request =
  stream conn request (view L.done)

mapReduce
  :: Interface -- ^
  -> RpbMapRedReq -- ^
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (RecvResult r)
mapReduce conn request =
  stream conn request (view L.done)

search
  :: Interface -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (RecvResult RpbSearchQueryResp)
search =
  exchange

setBucketProps
  :: Interface -- ^
  -> RpbSetBucketReq -- ^
  -> IO (RecvResult RpbSetBucketResp)
setBucketProps =
  exchange

setBucketTypeProps
  :: Interface -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (RecvResult RpbSetBucketTypeResp)
setBucketTypeProps =
  exchange

streamBuckets
  :: Interface -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (RecvResult r)
streamBuckets conn request =
  stream conn request (view L.done)

streamKeys
  :: Interface -- ^
  -> RpbListKeysReq -- ^
  -> FoldM IO RpbListKeysResp r -- ^
  -> IO (RecvResult r)
streamKeys conn request =
  stream conn request (view L.done)

updateCrdt
  :: Interface -- ^
  -> DtUpdateReq -- ^
  -> IO (RecvResult DtUpdateResp)
updateCrdt =
  exchange

