module Riak.Internal
  ( -- * Handle
    RiakHandle(..)
    -- * Connection manager
  , RiakManager
  , createRiakManager
  , withRiakConnection
    -- * Cache
  , RiakCache(..)
  , newSTMRiakCache
    -- * API
  , GetRiakCrdtParams(..)
  , GetRiakObjectParams(..)
  , PutRiakObjectParams(..)
  , RiakMapReduceInputs(..)
  , RiakMapReducePhase(..)
  , riakMapReducePhaseMapIdentity
  , riakMapReducePhaseMapObjectValue
  , riakMapReducePhaseReduceCount
  , riakMapReducePhaseReduceSetUnion
  , riakMapReducePhaseReduceSort
  , riakMapReducePhaseReduceSum
  , RiakSearchParams(..)
  , UpdateRiakCrdtParams(..)
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
  , CounterOp
  , DtFetchReq
  , DtFetchResp
  , DtFetchResp'DataType(..)
  , DtOp
  , DtUpdateReq
  , DtUpdateResp
  , DtValue
  , GSetOp
  , HllOp
  , MapEntry
  , MapField
  , MapField'MapFieldType
  , MapOp
  , MapUpdate
  , MapUpdate'FlagOp
  , RpbAuthReq
  , RpbBucketKeyPreflistItem
  , RpbBucketProps
  , RpbBucketProps'RpbReplMode
  , RpbCSBucketReq
  , RpbCSBucketResp
  , RpbCommitHook
  , RpbContent
  , RpbCounterGetReq
  , RpbCounterGetResp
  , RpbCounterUpdateReq
  , RpbCounterUpdateResp
  , RpbCoverageEntry
  , RpbCoverageReq
  , RpbCoverageResp
  , RpbDelReq
  , RpbDelResp
  , RpbEmptyPutResp
  , RpbErrorResp
  , RpbGetBucketKeyPreflistReq
  , RpbGetBucketKeyPreflistResp
  , RpbGetBucketReq
  , RpbGetBucketResp
  , RpbGetBucketTypeReq
  , RpbGetClientIdResp
  , RpbGetReq
  , RpbGetResp
  , RpbGetServerInfoResp
  , RpbIndexBodyResp
  , RpbIndexObject
  , RpbIndexReq
  , RpbIndexReq'IndexQueryType(..)
  , RpbIndexResp
  , RpbLink
  , RpbListBucketsReq
  , RpbListBucketsResp
  , RpbListKeysReq
  , RpbListKeysResp
  , RpbMapRedReq
  , RpbMapRedResp
  , RpbModFun
  , RpbPair
  , RpbPingResp
  , RpbPutReq
  , RpbPutResp
  , RpbResetBucketReq
  , RpbResetBucketResp
  , RpbSearchDoc
  , RpbSearchQueryReq
  , RpbSearchQueryResp
  , RpbSetBucketReq
  , RpbSetBucketResp
  , RpbSetBucketTypeReq
  , RpbSetBucketTypeResp
  , RpbSetClientIdReq
  , RpbYokozunaIndex
  , RpbYokozunaIndexDeleteReq
  , RpbYokozunaIndexGetReq
  , RpbYokozunaIndexGetResp
  , RpbYokozunaIndexPutReq
  , RpbYokozunaSchema
  , RpbYokozunaSchemaGetReq
  , RpbYokozunaSchemaGetResp
  , RpbYokozunaSchemaPutReq
  , SetOp
  , TsCell
  , TsColumnDescription
  , TsColumnType
  , TsCoverageEntry
  , TsCoverageReq
  , TsCoverageResp
  , TsDelReq
  , TsDelResp
  , TsGetReq
  , TsGetResp
  , TsInterpolation
  , TsListKeysReq
  , TsListKeysResp
  , TsPutReq
  , TsPutResp
  , TsQueryReq
  , TsQueryResp
  , TsRange
  , TsRow
  ) where

import Control.Foldl (FoldM)
import Lens.Labels   (view)

import qualified Control.Foldl as Foldl

import Riak.Internal.Cache
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
