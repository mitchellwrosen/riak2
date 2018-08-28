{-# LANGUAGE NoImplicitPrelude, OverloadedLabels #-}

module Riak.Internal
  ( -- * Handle
    -- * Connection manager
    -- * Connection
    RiakConnection
  , riakConnect
  , riakDisconnect
  , riakExchange
  , riakStream
    -- * Vclock cache
    -- * API
  , deleteRiakIndex
  , deleteRiakObject
  , fetchRiakCrdt
  , fetchRiakObject
  , getRiakBucketProps
  , getRiakBucketTypeProps
  , getRiakIndex
  , getRiakSchema
  , getRiakServerInfo
  , pingRiak
  , putRiakIndex
  , putRiakSchema
  , resetRiakBucketProps
  , riakMapReduce
  , setRiakBucketProps
  , setRiakBucketTypeProps
  , storeRiakObject
  , streamRiakBuckets
  , streamRiakKeys
  , updateRiakCrdt
    -- * Protocol buffers
  , CounterOp(..)
  , DtFetchReq(..)
  , DtFetchResp(..)
  , DtFetchResp'DataType(..)
  , DtOp(..)
  , DtUpdateReq(..)
  , DtUpdateResp(..)
  , DtValue(..)
  , GSetOp(..)
  , HllOp(..)
  , MapEntry(..)
  , MapField(..)
  , MapField'MapFieldType(..)
  , MapOp(..)
  , MapUpdate(..)
  , MapUpdate'FlagOp(..)
  , RpbAuthReq(..)
  , RpbBucketKeyPreflistItem(..)
  , RpbBucketProps(..)
  , RpbBucketProps'RpbReplMode(..)
  , RpbCSBucketReq(..)
  , RpbCSBucketResp(..)
  , RpbCommitHook(..)
  , RpbContent(..)
  , RpbCounterGetReq(..)
  , RpbCounterGetResp(..)
  , RpbCounterUpdateReq(..)
  , RpbCounterUpdateResp(..)
  , RpbCoverageEntry(..)
  , RpbCoverageReq(..)
  , RpbCoverageResp(..)
  , RpbDelReq(..)
  , RpbErrorResp(..)
  , RpbGetBucketKeyPreflistReq(..)
  , RpbGetBucketKeyPreflistResp(..)
  , RpbGetBucketReq(..)
  , RpbGetBucketResp(..)
  , RpbGetBucketTypeReq(..)
  , RpbGetClientIdResp(..)
  , RpbGetReq(..)
  , RpbGetResp(..)
  , RpbGetServerInfoResp(..)
  , RpbIndexBodyResp(..)
  , RpbIndexObject(..)
  , RpbIndexReq(..)
  , RpbIndexReq'IndexQueryType(..)
  , RpbIndexResp(..)
  , RpbLink(..)
  , RpbListBucketsReq(..)
  , RpbListBucketsResp(..)
  , RpbListKeysReq(..)
  , RpbListKeysResp(..)
  , RpbMapRedReq(..)
  , RpbMapRedResp(..)
  , RpbModFun(..)
  , RpbPair(..)
  , RpbPutReq(..)
  , RpbPutResp(..)
  , RpbResetBucketReq(..)
  , RpbSearchDoc(..)
  , RpbSearchQueryReq(..)
  , RpbSearchQueryResp(..)
  , RpbSetBucketReq(..)
  , RpbSetBucketTypeReq(..)
  , RpbSetClientIdReq(..)
  , RpbYokozunaIndex(..)
  , RpbYokozunaIndexDeleteReq(..)
  , RpbYokozunaIndexGetReq(..)
  , RpbYokozunaIndexGetResp(..)
  , RpbYokozunaIndexPutReq(..)
  , RpbYokozunaSchema(..)
  , RpbYokozunaSchemaGetReq(..)
  , RpbYokozunaSchemaGetResp(..)
  , RpbYokozunaSchemaPutReq(..)
  , SetOp(..)
  , TsCell(..)
  , TsColumnDescription(..)
  , TsColumnType(..)
  , TsCoverageEntry(..)
  , TsCoverageReq(..)
  , TsCoverageResp(..)
  , TsDelReq(..)
  , TsDelResp(..)
  , TsGetReq(..)
  , TsGetResp(..)
  , TsInterpolation(..)
  , TsListKeysReq(..)
  , TsListKeysResp(..)
  , TsPutReq(..)
  , TsPutResp(..)
  , TsQueryReq(..)
  , TsQueryResp(..)
  , TsRange(..)
  , TsRow(..)
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

streamRiakBuckets
  :: RiakConnection -- ^
  -> RpbListBucketsReq -- ^
  -> ListT (ExceptT RiakError IO) RpbListBucketsResp
streamRiakBuckets conn =
  riakStream conn (view #done)

streamRiakKeys
  :: RiakConnection -- ^
  -> RpbListKeysReq -- ^
  -> ListT (ExceptT RiakError IO) RpbListKeysResp
streamRiakKeys conn =
  riakStream conn (view #done)

updateRiakCrdt
  :: RiakConnection -- ^
  -> DtUpdateReq -- ^
  -> IO (Either RiakError DtUpdateResp)
updateRiakCrdt =
  riakExchange
