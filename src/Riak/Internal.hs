{-# LANGUAGE NoImplicitPrelude, OverloadedLabels, PatternSynonyms #-}

module Riak.Internal
  ( -- * Handle
    RiakHandle(..)
    -- * Connection manager
  , RiakManager(..)
  , createRiakManager
  , withRiakConnection
    -- * Connection
  , RiakConnection(..)
  , riakConnect
  , riakDisconnect
  , riakExchange
  , riakStream
  , Message(..)
  , Request(..)
  , requestToMessage
  , Response(..)
  , parseResponse
    -- * Cache
  , RiakCache(..)
  , newSTMRiakCache
    -- * API
  , deleteRiakIndexPB
  , deleteRiakObjectPB
  , getRiakBucketPropsPB
  , getRiakBucketTypePropsPB
  , GetRiakCrdtParams(..)
  , getRiakCrdtPB
  , getRiakIndexPB
  , GetRiakObjectParams(..)
  , getRiakObjectPB
  , getRiakSchemaPB
  , getRiakServerInfoPB
  , pingRiakPB
  , putRiakIndexPB
  , PutRiakObjectParams(..)
  , putRiakObjectPB
  , putRiakSchemaPB
  , resetRiakBucketPropsPB
  , riakIndexPB
  , RiakMapReduceInputs(..)
  , RiakMapReducePhase(..)
  , riakMapReducePB
  , riakMapReducePhaseMapIdentity
  , riakMapReducePhaseMapObjectValue
  , riakMapReducePhaseReduceCount
  , riakMapReducePhaseReduceIdentity
  , riakMapReducePhaseReduceSetUnion
  , riakMapReducePhaseReduceSort
  , riakMapReducePhaseReduceSum
  , RiakSearchParams(..)
  , riakSearchPB
  , setRiakBucketPropsPB
  , setRiakBucketTypePropsPB
  , streamRiakBucketsPB
  , streamRiakKeysPB
  , UpdateRiakCrdtParams(..)
  , updateRiakCrdtPB
    -- * Data types
  , RiakCrdtTy(..)
  , RiakCrdtError(..)
    -- ** Map
  , IsRiakMap(..)
  , RiakMapEntries(..)
  , RiakMapFieldParser
  , RiakMapParseError(..)
  , riakCounterField
  , riakFlagField
  , riakMapField
  , riakRegisterField
  , riakSetField
    -- ** Register
  , IsRiakRegister(..)
    -- ** Set
  , IsRiakSet
  , RiakSetOp(..)
  , riakSetAddOp
  , riakSetRemoveOp
    -- * Types
  , BasicQuorum(..)
  , Charset(..)
  , ContentEncoding(..)
  , ContentType(..)
  , DF(..)
  , DW(..)
  , Filter(..)
  , FL(..)
  , IncludeContext(..)
  , IsRiakContent(..)
  , JsonRiakContent(..)
  , Modified(..)
  , N(..)
  , NotfoundOk(..)
  , Op(..)
  , PR(..)
  , Presort(..)
  , PW(..)
  , R(..)
  , Rows(..)
  , ReturnBody(..)
  , RiakBucket(..)
  , pattern DefaultRiakBucket
  , RiakBucketType(..)
  , pattern DefaultRiakBucketType
  , RiakContent(..)
  , RiakError(..)
  , RiakExactQuery(..)
  , RiakIndex(..)
  , RiakIndexName(..)
  , RiakKey(..)
  , RiakMetadata(..)
  , RiakQuorum(..)
  , pattern RiakQuorumAll
  , pattern RiakQuorumQuorum
  , RiakRangeQuery(..)
  , RiakVclock(..)
  , RiakVtag(..)
  , SloppyQuorum(..)
  , SolrIndexName(..)
  , pattern DontIndex
  , SolrSchemaName(..)
  , pattern DefaultSolrSchemaName
  , Sort(..)
  , Start(..)
  , Timeout(..)
  , TTL(..)
  , W(..)
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
  , RpbDelResp(..)
  , RpbEmptyPutResp(..)
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
  , RpbPingResp(..)
  , RpbPutReq(..)
  , RpbPutResp(..)
  , RpbResetBucketReq(..)
  , RpbResetBucketResp(..)
  , RpbSearchDoc(..)
  , RpbSearchQueryReq(..)
  , RpbSearchQueryResp(..)
  , RpbSetBucketReq(..)
  , RpbSetBucketResp(..)
  , RpbSetBucketTypeReq(..)
  , RpbSetBucketTypeResp(..)
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
import Riak.Internal.Cache
import Riak.Internal.Connection
import Riak.Internal.Content
import Riak.Internal.Crdts
import Riak.Internal.Manager
import Riak.Internal.MapReduce
import Riak.Internal.Message
import Riak.Internal.Params
import Riak.Internal.Prelude
import Riak.Internal.Request
import Riak.Internal.Response
import Riak.Internal.Types


--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | A thread-safe handle to Riak.
--
-- TODO: RiakHandle improvement: cluster
data RiakHandle
  = RiakHandle !RiakManager !RiakCache


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

deleteRiakIndexPB
  :: RiakConnection
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (Either RiakError RpbDelResp)
deleteRiakIndexPB =
  riakExchange

deleteRiakObjectPB
  :: RiakConnection -- ^
  -> RpbDelReq -- ^
  -> IO (Either RiakError RpbDelResp)
deleteRiakObjectPB =
  riakExchange

getRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbGetBucketReq -- ^
  -> IO (Either RiakError RpbGetBucketResp)
getRiakBucketPropsPB =
  riakExchange

getRiakBucketTypePropsPB
  :: RiakConnection -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (Either RiakError RpbGetBucketResp)
getRiakBucketTypePropsPB =
  riakExchange

getRiakCrdtPB
  :: RiakConnection -- ^
  -> DtFetchReq -- ^
  -> IO (Either RiakError DtFetchResp)
getRiakCrdtPB
  = riakExchange

getRiakIndexPB
  :: RiakConnection -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (Either RiakError RpbYokozunaIndexGetResp)
getRiakIndexPB =
  riakExchange

getRiakObjectPB
  :: RiakConnection -- ^
  -> RpbGetReq -- ^
  -> IO (Either RiakError RpbGetResp)
getRiakObjectPB =
  riakExchange

getRiakSchemaPB
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (Either RiakError RpbYokozunaSchemaGetResp)
getRiakSchemaPB =
  riakExchange

getRiakServerInfoPB
  :: RiakConnection -- ^
  -> IO (Either RiakError RpbGetServerInfoResp)
getRiakServerInfoPB conn =
  riakExchange conn RpbGetServerInfoReq

pingRiakPB
  :: RiakConnection -- ^
  -> IO (Either RiakError RpbPingResp)
pingRiakPB conn =
  riakExchange conn RpbPingReq

putRiakIndexPB
  :: RiakConnection -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (Either RiakError RpbEmptyPutResp)
putRiakIndexPB =
  riakExchange

putRiakObjectPB
  :: RiakConnection -- ^
  -> RpbPutReq -- ^
  -> IO (Either RiakError RpbPutResp)
putRiakObjectPB =
  riakExchange

putRiakSchemaPB
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (Either RiakError RpbEmptyPutResp)
putRiakSchemaPB =
  riakExchange

resetRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbResetBucketReq -- ^
  -> IO (Either RiakError RpbResetBucketResp)
resetRiakBucketPropsPB =
  riakExchange

riakIndexPB
  :: RiakConnection -- ^
  -> RpbIndexReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbIndexResp -> IO r) -- ^
  -> IO r
riakIndexPB conn =
  riakStream conn (view #done)

riakMapReducePB
  :: RiakConnection -- ^
  -> RpbMapRedReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbMapRedResp -> IO r) -- ^
  -> IO r
riakMapReducePB conn =
  riakStream conn (view #done)

riakSearchPB
  :: RiakConnection -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (Either RiakError RpbSearchQueryResp)
riakSearchPB =
  riakExchange

setRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbSetBucketReq -- ^
  -> IO (Either RiakError RpbSetBucketResp)
setRiakBucketPropsPB =
  riakExchange

setRiakBucketTypePropsPB
  :: RiakConnection -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (Either RiakError RpbSetBucketTypeResp)
setRiakBucketTypePropsPB =
  riakExchange

streamRiakBucketsPB
  :: RiakConnection -- ^
  -> RpbListBucketsReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbListBucketsResp -> IO r) -- ^
  -> IO r
streamRiakBucketsPB conn =
  riakStream conn (view #done)

streamRiakKeysPB
  :: RiakConnection -- ^
  -> RpbListKeysReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbListKeysResp -> IO r) -- ^
  -> IO r
streamRiakKeysPB conn =
  riakStream conn (view #done)

updateRiakCrdtPB
  :: RiakConnection -- ^
  -> DtUpdateReq -- ^
  -> IO (Either RiakError DtUpdateResp)
updateRiakCrdtPB =
  riakExchange
