{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies,
             ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving,
             InstanceSigs, KindSignatures, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms #-}

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


-- TODO better BucketProps types
data BucketProps
  = BucketProps
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
newtype BucketType (ty :: Maybe DataTypeTy)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show (BucketType ty) where
  show :: BucketType ty -> String
  show =
    Text.unpack . decodeUtf8 . unBucketType

pattern BucketTypeDefault :: BucketType 'Nothing
pattern BucketTypeDefault =
  BucketType "default"


-- | A Riak bucket.
newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show Bucket where
  show :: Bucket -> String
  show (Bucket bucket) =
    either
      (const (Latin1.unpack (Base64.encode bucket)))
      Text.unpack
      (decodeUtf8' bucket)


data DataTypeTy
  = CounterTy
  | GrowOnlySetTy -- TODO better GrowOnlySetTy
  | HyperLogLogTy
  | MapTy -- TODO better MapTy
  | forall a. SetTy a


-- | A Solr index name.
newtype IndexName
  = IndexName { unIndexName :: ByteString }


-- | A Riak key.
newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show Key where
  show :: Key -> String
  show (Key key) =
    either
      (const (Latin1.unpack (Base64.encode key)))
      Text.unpack
      (decodeUtf8' key)


data Location (ty :: Maybe DataTypeTy)
  = Location !(Namespace ty) Key
  deriving stock (Eq, Show)

instance Hashable (Location ty) where
  hashWithSalt salt (Location namespace key) =
    salt `hashWithSalt` namespace `hashWithSalt` key


newtype Metadata
  = Metadata { unMetadata :: [(ByteString, Maybe ByteString)] }
  deriving (Show)


data Modified a
  = Unmodified
  | Modified a


data Namespace (ty :: Maybe DataTypeTy)
  = Namespace !(BucketType ty) !Bucket
  deriving stock (Eq, Show)

instance Hashable (Namespace ty) where
  hashWithSalt salt (Namespace type' bucket) =
    salt `hashWithSalt` type' `hashWithSalt` bucket


data ObjectReturn
  = ObjectReturnNone
  | ObjectReturnHead
  | ObjectReturnBody


-- | How many vnodes must respond before an operation is considered successful.
-- May be a number @<= N@, 'QuorumQuorum', or 'QuorumAll'.
newtype Quorum
  = Quorum { unQuorum :: Word32 }
  deriving stock (Eq)
  deriving newtype (Num)

instance Default Quorum where
  def = 4294967291

-- | All vnodes must respond.
pattern QuorumAll :: Quorum
pattern QuorumAll = 4294967292

-- | A majority of the vnodes must respond.
pattern QuorumQuorum :: Quorum
pattern QuorumQuorum = 4294967293


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False


data SecondaryIndex
  = SecondaryIndexInt !ByteString !Int64
  | SecondaryIndexBin !ByteString !ByteString
  deriving (Show)


newtype SecondaryIndexes
  = SecondaryIndexes { unSecondaryIndexes :: [SecondaryIndex] }
  deriving (Show)


data SomeLocation where
  SomeLocation :: Location ty -> SomeLocation
instance Eq SomeLocation where
  SomeLocation loc1 == SomeLocation loc2 =
    type1 == type2 && bucket1 == bucket2 && key1 == key2
   where
    Location (Namespace (BucketType type1) bucket1) key1 = loc1
    Location (Namespace (BucketType type2) bucket2) key2 = loc2

instance Hashable SomeLocation where
  hashWithSalt salt (SomeLocation loc) =
    hashWithSalt salt loc


newtype TTL
  = TTL { unTTL :: Maybe Word32 }
  deriving Show


newtype Vclock
  = Vclock { unVclock :: ByteString }

instance Show Vclock where
  show :: Vclock -> String
  show =
    show . Base64.encode . unVclock


newtype Vtag
  = Vtag { unVtag :: ByteString }
  deriving (Show)


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
