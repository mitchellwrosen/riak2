-- | Sin-bin of misc. types.

-- TODO $bucket index, only supports equality queries
-- TODO $key index, only supports range queries

module Riak.Internal.Types where

import Data.Default.Class
import Data.Hashable      (hashWithSalt)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Lens.Labels

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.Text              as Text

import Proto.Riak
import Riak.Internal.Prelude


data Modified a
  = Unmodified
  | Modified a
  deriving (Eq, Functor, Show)


data ObjectReturn
  = ObjectReturnNone
  | ObjectReturnHead
  | ObjectReturnBody


-- | A bucket type and bucket, tagged with the data type it contains.
data RiakBucket (ty :: Maybe RiakCrdtTy)
  = RiakBucket !(RiakBucketType ty) !ByteString
  deriving stock (Eq)

instance Hashable (RiakBucket ty) where
  hashWithSalt salt (RiakBucket type' bucket) =
    salt `hashWithSalt` type' `hashWithSalt` bucket

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show (RiakBucket ty) where
  show :: RiakBucket ty -> String
  show (RiakBucket type' bucket) =
    show type'
    ++ " " ++
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode bucket)))
      Text.unpack
      (decodeUtf8' bucket)

-- | (De)construct a bucket in the the @default@ bucket type. Its usage is
-- discouraged.
pattern DefaultRiakBucket :: ByteString -> RiakBucket 'Nothing
pattern DefaultRiakBucket name =
  RiakBucket DefaultRiakBucketType name


-- TODO better BucketProps types
data RiakBucketProps
  = RiakBucketProps
      !(Maybe Word32)                     -- n
      !(Maybe Bool)                       -- allow_mult
      !(Maybe Bool)                       -- last_write_wins
      ![RpbCommitHook]                    -- precommit
      !(Maybe Bool)                       -- has_precommit
      ![RpbCommitHook]                    -- postcommit
      !(Maybe Bool)                       -- has_postcommit
      !(Maybe Word32)                     -- old_vclock
      !(Maybe Word32)                     -- young_vclock
      !(Maybe Word32)                     -- big_vclock
      !(Maybe Word32)                     -- small_vclock
      !(Maybe Word32)                     -- pr
      !(Maybe Word32)                     -- r
      !(Maybe Word32)                     -- w
      !(Maybe Word32)                     -- pw
      !(Maybe Word32)                     -- dw
      !(Maybe Word32)                     -- rw
      !(Maybe Bool)                       -- basic_quorum
      !(Maybe Bool)                       -- notfound_ok
      !(Maybe ByteString)                 -- backend
      !(Maybe Bool)                       -- search
      !(Maybe RpbBucketProps'RpbReplMode) -- repl
      !(Maybe ByteString)                 -- search_index
      !(Maybe ByteString)                 -- datatype
      !(Maybe Bool)                       -- consistent
      !(Maybe Bool)                       -- write_once
      !(Maybe Word32)                     -- hll_precision
      !(Maybe Word32)                     -- ttl

-- | A bucket type, tagged with the data type it contains.
--
-- /Note/: Must be UTF-8 encoded.
newtype RiakBucketType (ty :: Maybe RiakCrdtTy)
  = RiakBucketType { unRiakBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show (RiakBucketType ty) where
  show :: RiakBucketType ty -> String
  show =
    Text.unpack . decodeUtf8 . unRiakBucketType

-- | (De)construct the @default@ bucket type. Its usage is discouraged.
pattern DefaultRiakBucketType :: RiakBucketType 'Nothing
pattern DefaultRiakBucketType =
  RiakBucketType "default"


data RiakCrdtTy
  = RiakCounterTy
  | RiakGrowOnlySetTy -- TODO better GrowOnlySetTy
  | RiakHyperLogLogTy
  | forall a. RiakMapTy a
  | forall a. RiakSetTy a


-- | Riak returned some error.
--
-- http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/#error-response
data RiakError
  = RiakError !Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)


-- | An exact query on a secondary index.
data RiakExactQuery
  = RiakExactQueryBin !RiakIndexName !ByteString
  | RiakExactQueryInt !RiakIndexName !Int64


-- TODO RiakIndex values should be a set
data RiakIndex
  = RiakIndexInt !RiakIndexName !Int64
  | RiakIndexBin !RiakIndexName !ByteString
  deriving (Eq, Show)


-- | A secondary index name.
newtype RiakIndexName
  = RiakIndexName { unRiakIndexName :: ByteString }
  deriving (Eq, Show)


-- | A bucket type, bucket, and key, tagged with the data type it contains.
data RiakKey (ty :: Maybe RiakCrdtTy)
  = RiakKey !(RiakBucket ty) !ByteString
  deriving stock (Eq)

instance Hashable (RiakKey ty) where
  hashWithSalt salt (RiakKey bucket key) =
    salt `hashWithSalt` bucket `hashWithSalt` key

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show (RiakKey ty) where
  show :: RiakKey ty -> String
  show (RiakKey bucket key) =
    show bucket
    ++ " " ++
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode key)))
      Text.unpack
      (decodeUtf8' key)


-- | Arbitrary metadata.
newtype RiakMetadata
  = RiakMetadata { unRiakMetadata :: [(ByteString, Maybe ByteString)] }
  deriving (Eq, Show)


-- TODO make RiakMapParseError a sub-exception of RiakParseError
data RiakParseError where
  RiakParseError :: !(RiakKey ty) -> !Text -> RiakParseError
  deriving anyclass Exception

deriving instance Show RiakParseError


-- | How many vnodes must respond before an operation is considered successful.
-- May be a number @<= N@, 'RiakQuorumQuorum', or 'RiakQuorumAll'.
newtype RiakQuorum
  = RiakQuorum { unRiakQuorum :: Word32 }
  deriving stock (Eq)
  deriving newtype (Num)

instance Default RiakQuorum where
  def = 4294967291

-- | All vnodes must respond.
pattern RiakQuorumAll :: RiakQuorum
pattern RiakQuorumAll = 4294967292

-- | A majority of the vnodes must respond.
pattern RiakQuorumQuorum :: RiakQuorum
pattern RiakQuorumQuorum = 4294967293


-- | A range query on a secondary index.
data RiakRangeQuery :: Type -> Type where
  RiakRangeQueryBin
    :: !RiakIndexName
    -> !ByteString
    -> !ByteString
    -> RiakRangeQuery ByteString

  RiakRangeQueryInt
    :: !RiakIndexName
    -> !Int64
    -> !Int64
    -> RiakRangeQuery Int64


newtype RiakVclock
  = RiakVclock { unRiakVclock :: ByteString }

instance Show RiakVclock where
  show :: RiakVclock -> String
  show =
    show . Base64.encode . unRiakVclock


newtype RiakVtag
  = RiakVtag { unRiakVtag :: ByteString }
  deriving (Eq, Show)


-- | A Solr index name.
--
-- /Note/: Must be ASCII.
newtype SolrIndexName
  = SolrIndexName { unSolrIndexName :: ByteString }

-- | @_dont_index_@
pattern DontIndex :: SolrIndexName
pattern DontIndex
  = SolrIndexName "_dont_index_"


-- | A Solr schema name.
newtype SolrSchemaName
  = SolrSchemaName { unSolrSchemaName :: ByteString }

-- | @_yz_default@
pattern DefaultSolrSchemaName :: SolrSchemaName
pattern DefaultSolrSchemaName =
  SolrSchemaName "_yz_default"


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False


data SomeRiakKey where
  SomeRiakKey :: RiakKey ty -> SomeRiakKey

instance Eq SomeRiakKey where
  SomeRiakKey (RiakKey n1 k1) == SomeRiakKey (RiakKey n2 k2) =
    t1 == t2 && b1 == b2 && k1 == k2
   where
    RiakBucket (RiakBucketType t1) b1 = n1
    RiakBucket (RiakBucketType t2) b2 = n2

instance Hashable SomeRiakKey where
  hashWithSalt salt (SomeRiakKey loc) =
    hashWithSalt salt loc


newtype TTL
  = TTL { unTTL :: Maybe Word32 }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Params
--------------------------------------------------------------------------------

data ParamObjectReturn :: ObjectReturn -> Type where
  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody
