-- | Sin-bin of misc. types.

-- TODO $bucket index, only supports equality queries
-- TODO $key index, only supports range queries

module ZZZ.Riak.Internal.Types where

import Data.Default.Class
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
-- import Lens.Labels

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.Text              as Text

import Riak.Internal.Prelude
import Riak.Proto


--data Modified a
--  = Unmodified
--  | Modified a
--  deriving (Eq, Functor, Show)


--data ObjectReturn
--  = ObjectReturnNone
--  | ObjectReturnHead
--  | ObjectReturnBody


---- | A bucket type and bucket.
--data RiakBucket
--  = RiakBucket !RiakBucketType !ByteString
--  deriving stock (Eq, Generic)
--  deriving anyclass (Hashable)

---- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
---- base64-encoding.
--instance Show RiakBucket where
--  show :: RiakBucket -> String
--  show (RiakBucket type' bucket) =
--    show type'
--    ++ " " ++
--    either
--      (const ("base64:" ++ Latin1.unpack (Base64.encode bucket)))
--      Text.unpack
--      (decodeUtf8' bucket)

---- | (De)construct a bucket in the the @default@ bucket type. Its usage is
---- discouraged.
--pattern DefaultRiakBucket :: ByteString -> RiakBucket
--pattern DefaultRiakBucket name =
--  RiakBucket DefaultRiakBucketType name


---- TODO better BucketProps types
--data RiakBucketProps
--  = RiakBucketProps
--      !(Maybe Word32)                     -- n
--      !(Maybe Bool)                       -- allow_mult
--      !(Maybe Bool)                       -- last_write_wins
--      ![CommitHook]                    -- precommit
--      !(Maybe Bool)                       -- has_precommit
--      ![RpbCommitHook]                    -- postcommit
--      !(Maybe Bool)                       -- has_postcommit
--      !(Maybe Word32)                     -- old_vclock
--      !(Maybe Word32)                     -- young_vclock
--      !(Maybe Word32)                     -- big_vclock
--      !(Maybe Word32)                     -- small_vclock
--      !(Maybe Word32)                     -- pr
--      !(Maybe Word32)                     -- r
--      !(Maybe Word32)                     -- w
--      !(Maybe Word32)                     -- pw
--      !(Maybe Word32)                     -- dw
--      !(Maybe Word32)                     -- rw
--      !(Maybe Bool)                       -- basic_quorum
--      !(Maybe Bool)                       -- notfound_ok
--      !(Maybe ByteString)                 -- backend
--      !(Maybe Bool)                       -- search
--      !(Maybe RpbBucketProps'RpbReplMode) -- repl
--      !(Maybe ByteString)                 -- search_index
--      !(Maybe ByteString)                 -- datatype
--      !(Maybe Bool)                       -- consistent
--      !(Maybe Bool)                       -- write_once
--      !(Maybe Word32)                     -- hll_precision
--      !(Maybe Word32)                     -- ttl

---- | A bucket type.
----
---- /Note/: Must be UTF-8 encoded.
--newtype RiakBucketType
--  = RiakBucketType { unRiakBucketType :: ByteString }
--  deriving stock (Eq)
--  deriving newtype (Hashable)

--instance Show RiakBucketType where
--  show :: RiakBucketType -> String
--  show =
--    Text.unpack . decodeUtf8 . unRiakBucketType

---- | (De)construct the @default@ bucket type. Its usage is discouraged.
--pattern DefaultRiakBucketType :: RiakBucketType
--pattern DefaultRiakBucketType =
--  RiakBucketType "default"


---- | Riak returned some error.
----
---- http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/#error-response
--data RiakError
--  = RiakError !Text
--  deriving stock (Eq, Show)
--  deriving anyclass (Exception)
---- -- Code as of 2.2.3 is currently always 0, so just just toss it
---- toRiakError :: RpbErrorResp -> RiakError
---- toRiakError resp =
----   RiakError (decodeUtf8 (resp ^. #errmsg))


---- | An exact query on a secondary index.
--data RiakExactQuery
--  = RiakExactQueryBin !RiakIndexName !ByteString
--  | RiakExactQueryInt !RiakIndexName !Int64


---- TODO RiakIndex values should be a set
--data RiakIndex
--  = RiakIndexInt !RiakIndexName !Int64
--  | RiakIndexBin !RiakIndexName !ByteString
--  deriving (Eq, Show)


---- | A secondary index name.
--newtype RiakIndexName
--  = RiakIndexName { unRiakIndexName :: ByteString }
--  deriving (Eq, Show)


---- | A bucket type, bucket, and key
--data RiakKey
--  = RiakKey !RiakBucket !ByteString
--  deriving stock (Eq, Generic)
--  deriving anyclass (Hashable)

---- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
---- base64-encoding.
--instance Show RiakKey where
--  show :: RiakKey -> String
--  show (RiakKey bucket key) =
--    show bucket
--    ++ " " ++
--    either
--      (const ("base64:" ++ Latin1.unpack (Base64.encode key)))
--      Text.unpack
--      (decodeUtf8' key)


---- | Arbitrary metadata.
--newtype RiakMetadata
--  = RiakMetadata { unRiakMetadata :: [(ByteString, Maybe ByteString)] }
--  deriving (Eq, Show)


---- | How many vnodes must respond before an operation is considered successful.
---- May be a number @<= N@, 'RiakQuorumQuorum', or 'RiakQuorumAll'.
--newtype RiakQuorum
--  = RiakQuorum { unRiakQuorum :: Word32 }
--  deriving stock (Eq)
--  deriving newtype (Num)

--instance Default RiakQuorum where
--  def = RiakQuorumDefault

--instance Show RiakQuorum where
--  show RiakQuorumAll     = "all"
--  show RiakQuorumDefault = "default"
--  show RiakQuorumQuorum  = "quorum"
--  show (RiakQuorum n)    = show n

---- | All vnodes must respond.
--pattern RiakQuorumAll :: RiakQuorum
--pattern RiakQuorumAll = 4294967292

--pattern RiakQuorumDefault :: RiakQuorum
--pattern RiakQuorumDefault = 4294967291

---- | A majority of the vnodes must respond.
--pattern RiakQuorumQuorum :: RiakQuorum
--pattern RiakQuorumQuorum = 4294967293


---- | A range query on a secondary index.
--data RiakRangeQuery :: Type -> Type where
--  RiakRangeQueryBin
--    :: !RiakIndexName
--    -> !ByteString
--    -> !ByteString
--    -> RiakRangeQuery ByteString

--  RiakRangeQueryInt
--    :: !RiakIndexName
--    -> !Int64
--    -> !Int64
--    -> RiakRangeQuery Int64


--data RiakTy
--  = RiakCounterTy
--  | RiakGrowOnlySetTy -- TODO better GrowOnlySetTy
--  | RiakHyperLogLogTy
--  | forall a. RiakMapTy a
--  | forall a. RiakSetTy a


--newtype RiakVclock
--  = RiakVclock { unRiakVclock :: ByteString }
--  deriving (Eq)

--instance Show RiakVclock where
--  show :: RiakVclock -> String
--  show =
--    show . Base64.encode . unRiakVclock


--newtype RiakVtag
--  = RiakVtag { unRiakVtag :: ByteString }
--  deriving (Eq, Show)


---- | A Solr index name.
----
---- /Note/: Must be ASCII.
--newtype SolrIndexName
--  = SolrIndexName { unSolrIndexName :: ByteString }

---- | @_dont_index_@
--pattern DontIndex :: SolrIndexName
--pattern DontIndex
--  = SolrIndexName "_dont_index_"


---- | A Solr schema name.
--newtype SolrSchemaName
--  = SolrSchemaName { unSolrSchemaName :: ByteString }

---- | @_yz_default@
--pattern DefaultSolrSchemaName :: SolrSchemaName
--pattern DefaultSolrSchemaName =
--  SolrSchemaName "_yz_default"


--data SBool :: Bool -> Type where
--  STrue  :: SBool 'True
--  SFalse :: SBool 'False


--newtype TTL
--  = TTL { unTTL :: Maybe Word32 }
--  deriving (Eq, Show)


----------------------------------------------------------------------------------
---- Params
----------------------------------------------------------------------------------

--data ParamObjectReturn :: ObjectReturn -> Type where
--  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
--  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
--  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody
