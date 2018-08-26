{-# LANGUAGE NoImplicitPrelude, OverloadedLabels #-}

module Riak.Internal
  ( RiakConnection
  , riakConnect
  , riakDisconnect
  , deleteRiakIndex
  , deleteRiakObject
  , fetchRiakCrdt
  , fetchRiakObject
  , getRiakBucketProps
  , getRiakBucketTypeProps
  , getRiakIndex
  , getRiakSchema
  , getRiakServerInfo
  , listRiakBuckets
  , listRiakKeys
  , pingRiak
  , putRiakIndex
  , putRiakSchema
  , resetRiakBucketProps
  , riakMapReduce
  , setRiakBucketProps
  , setRiakBucketTypeProps
  , storeRiakObject
  , updateRiakCrdt
  ) where

import Lens.Labels (view)

import Proto.Riak
import Riak.Internal.Connection
import Riak.Internal.Prelude
import Riak.Internal.Request
import Riak.Internal.Response
import Riak.Internal.Types

deleteRiakIndex
  :: RiakConnection
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (Either RiakError RpbDelResp)
deleteRiakIndex =
  riakExchange

deleteRiakObject
  :: RiakConnection -- ^
  -> RpbDelReq -- ^
  -> IO (Either RiakError RpbDelResp)
deleteRiakObject =
  riakExchange

fetchRiakCrdt
  :: RiakConnection -- ^
  -> DtFetchReq -- ^
  -> IO (Either RiakError DtFetchResp)
fetchRiakCrdt
  = riakExchange

fetchRiakObject
  :: RiakConnection -- ^
  -> RpbGetReq -- ^
  -> IO (Either RiakError RpbGetResp)
fetchRiakObject =
  riakExchange

getRiakBucketProps
  :: RiakConnection -- ^
  -> RpbGetBucketReq -- ^
  -> IO (Either RiakError RpbGetBucketResp)
getRiakBucketProps =
  riakExchange

getRiakBucketTypeProps
  :: RiakConnection -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (Either RiakError RpbGetBucketResp)
getRiakBucketTypeProps =
  riakExchange

getRiakIndex
  :: RiakConnection -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (Either RiakError RpbYokozunaIndexGetResp)
getRiakIndex =
  riakExchange

getRiakSchema
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (Either RiakError RpbYokozunaSchemaGetResp)
getRiakSchema =
  riakExchange

getRiakServerInfo
  :: RiakConnection -- ^
  -> IO (Either RiakError RpbGetServerInfoResp)
getRiakServerInfo conn =
  riakExchange conn RpbGetServerInfoReq

listRiakBuckets
  :: RiakConnection -- ^
  -> RpbListBucketsReq -- ^
  -> ListT (ExceptT RiakError IO) RpbListBucketsResp
listRiakBuckets conn =
  riakStream conn (view #done)

listRiakKeys
  :: RiakConnection -- ^
  -> RpbListKeysReq -- ^
  -> ListT (ExceptT RiakError IO) RpbListKeysResp
listRiakKeys conn =
  riakStream conn (view #done)

pingRiak
  :: RiakConnection -- ^
  -> IO (Either RiakError RpbPingResp)
pingRiak conn =
  riakExchange conn RpbPingReq

putRiakIndex
  :: RiakConnection -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (Either RiakError RpbEmptyPutResp)
putRiakIndex =
  riakExchange

putRiakSchema
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (Either RiakError RpbEmptyPutResp)
putRiakSchema =
  riakExchange

resetRiakBucketProps
  :: RiakConnection -- ^
  -> RpbResetBucketReq -- ^
  -> IO (Either RiakError RpbResetBucketResp)
resetRiakBucketProps =
  riakExchange

riakMapReduce
  :: RiakConnection -- ^
  -> RpbMapRedReq -- ^
  -> ListT (ExceptT RiakError IO) RpbMapRedResp
riakMapReduce conn =
  riakStream conn (view #done)

setRiakBucketProps
  :: RiakConnection -- ^
  -> RpbSetBucketReq -- ^
  -> IO (Either RiakError RpbSetBucketResp)
setRiakBucketProps =
  riakExchange

setRiakBucketTypeProps
  :: RiakConnection -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (Either RiakError RpbSetBucketTypeResp)
setRiakBucketTypeProps =
  riakExchange

storeRiakObject
  :: RiakConnection -- ^
  -> RpbPutReq -- ^
  -> IO (Either RiakError RpbPutResp)
storeRiakObject =
  riakExchange

updateRiakCrdt
  :: RiakConnection -- ^
  -> DtUpdateReq -- ^
  -> IO (Either RiakError DtUpdateResp)
updateRiakCrdt =
  riakExchange
