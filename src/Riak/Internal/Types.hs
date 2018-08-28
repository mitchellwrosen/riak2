{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DerivingStrategies,
             ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving,
             InstanceSigs, KindSignatures, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, StandaloneDeriving #-}

-- | Sin-bin of misc. types.

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


-- | A Riak bucket.
newtype RiakBucket
  = RiakBucket { unRiakBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show RiakBucket where
  show :: RiakBucket -> String
  show (RiakBucket bucket) =
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode bucket)))
      Text.unpack
      (decodeUtf8' bucket)


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

-- | A Riak bucket type, tagged with the data type it contains.
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


-- | A Solr index name.
newtype RiakIndexName
  = RiakIndexName { unRiakIndexName :: ByteString }


-- | A Riak key.
newtype RiakKey
  = RiakKey { unRiakKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show RiakKey where
  show :: RiakKey -> String
  show (RiakKey key) =
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode key)))
      Text.unpack
      (decodeUtf8' key)


data RiakLocation (ty :: Maybe RiakCrdtTy)
  = RiakLocation !(RiakNamespace ty) !RiakKey
  deriving stock (Eq, Show)

instance Hashable (RiakLocation ty) where
  hashWithSalt salt (RiakLocation namespace key) =
    salt `hashWithSalt` namespace `hashWithSalt` key


newtype RiakMetadata
  = RiakMetadata { unRiakMetadata :: [(ByteString, Maybe ByteString)] }
  deriving (Eq, Show)


data RiakNamespace (ty :: Maybe RiakCrdtTy)
  = RiakNamespace !(RiakBucketType ty) !RiakBucket
  deriving stock (Eq, Show)

instance Hashable (RiakNamespace ty) where
  hashWithSalt salt (RiakNamespace type' bucket) =
    salt `hashWithSalt` type' `hashWithSalt` bucket


-- TODO make RiakMapParseError a sub-exception of RiakParseError
data RiakParseError where
  RiakParseError :: !(RiakLocation ty) -> !Text -> RiakParseError
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


-- | A Solr schema name.
newtype RiakSchemaName
  = RiakSchemaName { unRiakSchemaName :: ByteString }

pattern DefaultRiakSchemaName :: RiakSchemaName
pattern DefaultRiakSchemaName =
  RiakSchemaName "_yz_default"


data RiakSecondaryIndex
  = RiakSecondaryIndexInt !ByteString !Int64
  | RiakSecondaryIndexBin !ByteString !ByteString
  deriving (Eq, Show)


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False


data SomeRiakLocation where
  SomeRiakLocation :: RiakLocation ty -> SomeRiakLocation

instance Eq SomeRiakLocation where
  SomeRiakLocation loc1 == SomeRiakLocation loc2 =
    type1 == type2 && bucket1 == bucket2 && key1 == key2
   where
    RiakLocation (RiakNamespace (RiakBucketType type1) bucket1) key1 = loc1
    RiakLocation (RiakNamespace (RiakBucketType type2) bucket2) key2 = loc2

instance Hashable SomeRiakLocation where
  hashWithSalt salt (SomeRiakLocation loc) =
    hashWithSalt salt loc


newtype TTL
  = TTL { unTTL :: Maybe Word32 }
  deriving (Eq, Show)


newtype RiakVclock
  = RiakVclock { unRiakVclock :: ByteString }

instance Show RiakVclock where
  show :: RiakVclock -> String
  show =
    show . Base64.encode . unRiakVclock


newtype RiakVtag
  = RiakVtag { unRiakVtag :: ByteString }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Params
--------------------------------------------------------------------------------

data Head a :: Bool -> Type where
  Head   :: Head a 'True
  NoHead :: Head a 'False


data IfModified :: Bool -> Type where
  IfModified   :: IfModified 'True
  NoIfModified :: IfModified 'False

instance (a ~ 'False) => Default (IfModified a) where
  def = NoIfModified


data ParamObjectReturn :: ObjectReturn -> Type where
  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody
