module Riak.Client
  ( Client
  , Client.connect
  , Client.disconnect
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

import Riak.Client.Signature (Client)
import Riak.Internal.Prelude
import Riak.Message          (Message)
import Riak.Request          (Request)
import Riak.Response         (Response)

import qualified Riak.Client.Signature as Client
import qualified Riak.Request          as Request
import qualified Riak.Response         as Response

import Riak.Proto
import Riak.Proto.Lens (done)

import Control.Foldl (FoldM(..))
import Control.Lens  (view)


--------------------------------------------------------------------------------
-- riak2-client porcelain
--------------------------------------------------------------------------------

data RecvResult a
  = RiakClosedConnection
  | Failure RpbErrorResp
  | Success a
  deriving stock (Eq, Functor, Show)

exchange ::
     forall a b.
     (Request a, Response b)
  => Client
  -> a
  -> IO (RecvResult b)
exchange client request =
  Client.exchange client (Request.toMessage request) >>=
    toRecvResult

stream ::
     forall a b r.
     (Request a, Response b)
  => Client
  -> a -- ^ Request
  -> (b -> Bool) -- ^ Done?
  -> FoldM IO b r -- ^ Fold responses
  -> IO (RecvResult r)
stream client request done (FoldM step initial extract) =
  Client.stream
    client
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
  :: Client -- ^
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (RecvResult RpbDelResp)
deleteIndex =
  exchange

deleteObject
  :: Client -- ^
  -> RpbDelReq -- ^
  -> IO (RecvResult RpbDelResp)
deleteObject =
  exchange

getBucketProps
  :: Client -- ^
  -> RpbGetBucketReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketProps =
  exchange

getBucketTypeProps
  :: Client -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketTypeProps =
  exchange

getCrdt
  :: Client -- ^
  -> DtFetchReq -- ^
  -> IO (RecvResult DtFetchResp)
getCrdt
  = exchange

getIndex
  :: Client -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (RecvResult RpbYokozunaIndexGetResp)
getIndex =
  exchange

getObject
  :: Client -- ^
  -> RpbGetReq -- ^
  -> IO (RecvResult RpbGetResp)
getObject =
  exchange

getSchema
  :: Client -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (RecvResult RpbYokozunaSchemaGetResp)
getSchema =
  exchange

getServerInfo
  :: Client -- ^
  -> IO (RecvResult RpbGetServerInfoResp)
getServerInfo conn =
  exchange conn RpbGetServerInfoReq

ping
  :: Client -- ^
  -> IO (RecvResult RpbPingResp)
ping conn = do
  exchange conn RpbPingReq

putIndex
  :: Client -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putIndex =
  exchange

putObject
  :: Client -- ^
  -> RpbPutReq -- ^
  -> IO (RecvResult RpbPutResp)
putObject =
  exchange

putSchema
  :: Client -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putSchema =
  exchange

resetBucketProps
  :: Client -- ^
  -> RpbResetBucketReq -- ^
  -> IO (RecvResult RpbResetBucketResp)
resetBucketProps =
  exchange

index
  :: Client -- ^
  -> RpbIndexReq -- ^
  -> FoldM IO RpbIndexResp r -- ^
  -> IO (RecvResult r)
index conn request =
  stream conn request (view done)

mapReduce
  :: Client -- ^
  -> RpbMapRedReq -- ^
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (RecvResult r)
mapReduce conn request =
  stream conn request (view done)

search
  :: Client -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (RecvResult RpbSearchQueryResp)
search =
  exchange

setBucketProps
  :: Client -- ^
  -> RpbSetBucketReq -- ^
  -> IO (RecvResult RpbSetBucketResp)
setBucketProps =
  exchange

setBucketTypeProps
  :: Client -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (RecvResult RpbSetBucketTypeResp)
setBucketTypeProps =
  exchange

streamBuckets
  :: Client -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (RecvResult r)
streamBuckets conn request =
  stream conn request (view done)

streamKeys
  :: Client -- ^
  -> RpbListKeysReq -- ^
  -> FoldM IO RpbListKeysResp r -- ^
  -> IO (RecvResult r)
streamKeys conn request =
  stream conn request (view done)

updateCrdt
  :: Client -- ^
  -> DtUpdateReq -- ^
  -> IO (RecvResult DtUpdateResp)
updateCrdt =
  exchange
