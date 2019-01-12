module Riak.Internal
  ( -- * Handle
    RiakHandle(..)
    -- * Connection manager
  , RiakManager
  , createRiakManager
  , withRiakConnection
    -- * Connection
  , RiakConnection(..)
  , riakConnect
  , riakDisconnect
  , riakExchange
  , riakStream
  -- , Message(..)
  -- , Request(..)
  -- , requestToMessage
  -- , Response(..)
  -- , parseResponse
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
  , putRiakObjectPB_
  , putRiakSchemaPB
  , resetRiakBucketPropsPB
  , riakIndexPB
  , RiakMapReduceInputs(..)
  , RiakMapReducePhase(..)
  , riakMapReducePB
  , riakMapReducePhaseMapIdentity
  , riakMapReducePhaseMapObjectValue
  , riakMapReducePhaseReduceCount
  , riakMapReducePhaseReduceSetUnion
  , riakMapReducePhaseReduceSort
  , riakMapReducePhaseReduceSum
  , RiakSearchParams(..)
  , riakSearchPB
  , setRiakBucketPropsPB
  , setRiakBucketTypePropsPB
  , streamRiakBucketsPB
  , streamRiakBuckets2PB
  , streamRiakKeysPB
  , UpdateRiakCrdtParams(..)
  , updateRiakCrdtPB
    -- * Data types
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
  , IsRiakObject(..)
  , JsonRiakObject(..)
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
  , RiakError(..)
  , RiakExactQuery(..)
  , RiakIndex(..)
  , RiakIndexName(..)
  , RiakKey(..)
  , RiakMetadata(..)
  , RiakObject(..)
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

import Control.Foldl (FoldM)
import Lens.Labels   (view)

import qualified Control.Foldl as Foldl

import Riak.Internal.Cache
import Riak.Internal.Connection
import Riak.Internal.Crdts
import Riak.Internal.Manager
import Riak.Internal.MapReduce
import Riak.Internal.Object
import Riak.Internal.Params
import Riak.Internal.Prelude
import Riak.Internal.Types
import Riak.Proto


--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | A thread-safe handle to Riak.
--
-- TODO: RiakHandle optional cache
data RiakHandle
  = RiakHandle !RiakManager !RiakCache


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

deleteRiakIndexPB
  :: RiakConnection
  -> RpbYokozunaIndexDeleteReq -- ^
  -> IO (Either RpbErrorResp RpbDelResp)
deleteRiakIndexPB =
  riakExchange

deleteRiakObjectPB
  :: RiakConnection -- ^
  -> RpbDelReq -- ^
  -> IO (Either RpbErrorResp RpbDelResp)
deleteRiakObjectPB =
  riakExchange

getRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbGetBucketReq -- ^
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getRiakBucketPropsPB =
  riakExchange

getRiakBucketTypePropsPB
  :: RiakConnection -- ^
  -> RpbGetBucketTypeReq -- ^
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getRiakBucketTypePropsPB =
  riakExchange

getRiakCrdtPB
  :: RiakConnection -- ^
  -> DtFetchReq -- ^
  -> IO (Either RpbErrorResp DtFetchResp)
getRiakCrdtPB
  = riakExchange

getRiakIndexPB
  :: RiakConnection -- ^
  -> RpbYokozunaIndexGetReq -- ^
  -> IO (Either RpbErrorResp RpbYokozunaIndexGetResp)
getRiakIndexPB =
  riakExchange

getRiakObjectPB
  :: RiakConnection -- ^
  -> RpbGetReq -- ^
  -> IO (Either RpbErrorResp RpbGetResp)
getRiakObjectPB =
  riakExchange

getRiakSchemaPB
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaGetReq -- ^
  -> IO (Either RpbErrorResp RpbYokozunaSchemaGetResp)
getRiakSchemaPB =
  riakExchange

getRiakServerInfoPB
  :: RiakConnection -- ^
  -> IO (Either RpbErrorResp RpbGetServerInfoResp)
getRiakServerInfoPB conn =
  riakExchange conn RpbGetServerInfoReq

pingRiakPB
  :: RiakConnection -- ^
  -> IO (Either RpbErrorResp RpbPingResp)
pingRiakPB conn =
  riakExchange conn RpbPingReq

putRiakIndexPB
  :: RiakConnection -- ^
  -> RpbYokozunaIndexPutReq -- ^
  -> IO (Either RpbErrorResp RpbEmptyPutResp)
putRiakIndexPB =
  riakExchange

putRiakObjectPB
  :: RiakConnection -- ^
  -> RpbPutReq -- ^
  -> IO (Either RpbErrorResp RpbPutResp)
putRiakObjectPB =
  riakExchange

putRiakObjectPB_
  :: RiakConnection -- ^
  -> RpbPutReq -- ^
  -> IO (Either RpbErrorResp ())
putRiakObjectPB_ =
  riakExchange_ @RpbPutResp

putRiakSchemaPB
  :: RiakConnection -- ^
  -> RpbYokozunaSchemaPutReq -- ^
  -> IO (Either RpbErrorResp RpbEmptyPutResp)
putRiakSchemaPB =
  riakExchange

resetRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbResetBucketReq -- ^
  -> IO (Either RpbErrorResp RpbResetBucketResp)
resetRiakBucketPropsPB =
  riakExchange

riakIndexPB
  :: RiakConnection -- ^
  -> RpbIndexReq -- ^
  -> FoldM IO RpbIndexResp r -- ^
  -> IO (Either RpbErrorResp r)
riakIndexPB conn request =
  Foldl.impurely (riakStream conn (view #done) request)

riakMapReducePB
  :: RiakConnection -- ^
  -> RpbMapRedReq -- ^
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RpbErrorResp r)
riakMapReducePB conn request =
  Foldl.impurely (riakStream conn (view #done) request)

riakSearchPB
  :: RiakConnection -- ^
  -> RpbSearchQueryReq -- ^
  -> IO (Either RpbErrorResp RpbSearchQueryResp)
riakSearchPB =
  riakExchange

setRiakBucketPropsPB
  :: RiakConnection -- ^
  -> RpbSetBucketReq -- ^
  -> IO (Either RpbErrorResp RpbSetBucketResp)
setRiakBucketPropsPB =
  riakExchange

setRiakBucketTypePropsPB
  :: RiakConnection -- ^
  -> RpbSetBucketTypeReq -- ^
  -> IO (Either RpbErrorResp RpbSetBucketTypeResp)
setRiakBucketTypePropsPB =
  riakExchange

streamRiakBucketsPB
  :: RiakConnection -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (Either RpbErrorResp r)
streamRiakBucketsPB conn request =
  Foldl.impurely (riakStream conn (view #done) request)

streamRiakBuckets2PB
  :: RiakConnection -- ^
  -> RpbListBucketsReq -- ^
  -> FoldM IO RpbListBucketsResp r -- ^
  -> IO (Either RpbErrorResp r)
streamRiakBuckets2PB conn request =
  Foldl.impurely (riakStream conn (view #done) request)

streamRiakKeysPB
  :: RiakConnection -- ^
  -> RpbListKeysReq -- ^
  -> FoldM IO RpbListKeysResp r -- ^
  -> IO (Either RpbErrorResp r)
streamRiakKeysPB conn request =
  Foldl.impurely (riakStream conn (view #done) request)

updateRiakCrdtPB
  :: RiakConnection -- ^
  -> DtUpdateReq -- ^
  -> IO (Either RpbErrorResp DtUpdateResp)
updateRiakCrdtPB =
  riakExchange
