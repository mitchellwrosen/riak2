module Riak.Client
  ( deleteIndex
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
  ) where

import Riak.Proto
import Riak.Proto.Lens        (done)
import Riak.Socket.Concurrent (RecvResult(..), Socket)

import qualified Riak.Socket.Concurrent as Socket

import Control.Foldl (FoldM)
import Control.Lens  (view)


deleteIndex
  :: Socket -- ^
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (RecvResult RpbDelResp)
deleteIndex =
  Socket.exchange

deleteObject
  :: Socket -- ^
  -> RpbDelReq -- ^
  -> IO (RecvResult RpbDelResp)
deleteObject =
  Socket.exchange

getBucketProps
  :: Socket -- ^
  -> RpbGetBucketReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketProps =
  Socket.exchange

getBucketTypeProps
  :: Socket -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (RecvResult RpbGetBucketResp)
getBucketTypeProps =
  Socket.exchange

getCrdt
  :: Socket -- ^
  -> DtFetchReq -- ^
  -> IO (RecvResult DtFetchResp)
getCrdt
  = Socket.exchange

getIndex
  :: Socket -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (RecvResult RpbYokozunaIndexGetResp)
getIndex =
  Socket.exchange

getObject
  :: Socket -- ^
  -> RpbGetReq -- ^
  -> IO (RecvResult RpbGetResp)
getObject =
  Socket.exchange

getSchema
  :: Socket -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (RecvResult RpbYokozunaSchemaGetResp)
getSchema =
  Socket.exchange

getServerInfo
  :: Socket -- ^
  -> IO (RecvResult RpbGetServerInfoResp)
getServerInfo conn =
  Socket.exchange conn RpbGetServerInfoReq

ping
  :: Socket -- ^
  -> IO (RecvResult RpbPingResp)
ping conn = do
  Socket.exchange conn RpbPingReq

putIndex
  :: Socket -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putIndex =
  Socket.exchange

putObject
  :: Socket -- ^
  -> RpbPutReq -- ^
  -> IO (RecvResult RpbPutResp)
putObject =
  Socket.exchange

-- putObject_
--   :: Socket -- ^
--   -> RpbPutReq -- ^
--   -> IO (RecvResult ())
-- putRiakObjectPB_ =
--   Socket.exchange_ @RpbPutResp

putSchema
  :: Socket -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (RecvResult RpbEmptyPutResp)
putSchema =
  Socket.exchange

resetBucketProps
  :: Socket -- ^
  -> RpbResetBucketReq -- ^
  -> IO (RecvResult RpbResetBucketResp)
resetBucketProps =
  Socket.exchange

index
  :: Socket -- ^
  -> RpbIndexReq -- ^
  -> FoldM IO RpbIndexResp r -- ^
  -> IO (RecvResult r)
index conn request =
  Socket.stream conn request (view done)

mapReduce
  :: Socket -- ^
  -> RpbMapRedReq -- ^
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (RecvResult r)
mapReduce conn request =
  Socket.stream conn request (view done)

search
  :: Socket -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (RecvResult RpbSearchQueryResp)
search =
  Socket.exchange

setBucketProps
  :: Socket -- ^
  -> RpbSetBucketReq -- ^
  -> IO (RecvResult RpbSetBucketResp)
setBucketProps =
  Socket.exchange

setBucketTypeProps
  :: Socket -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (RecvResult RpbSetBucketTypeResp)
setBucketTypeProps =
  Socket.exchange

streamBuckets
  :: Socket -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (RecvResult r)
streamBuckets conn request =
  Socket.stream conn request (view done)

streamKeys
  :: Socket -- ^
  -> RpbListKeysReq -- ^
  -> FoldM IO RpbListKeysResp r -- ^
  -> IO (RecvResult r)
streamKeys conn request =
  Socket.stream conn request (view done)

updateCrdt
  :: Socket -- ^
  -> DtUpdateReq -- ^
  -> IO (RecvResult DtUpdateResp)
updateCrdt =
  Socket.exchange
