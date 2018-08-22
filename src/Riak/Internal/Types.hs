{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, GADTs,
             GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
             OverloadedStrings, PatternSynonyms #-}

-- | Sin-bin of misc. types.

module Riak.Internal.Types where

import Data.ByteString    (ByteString)
import Data.Default.Class
import Data.Hashable      (Hashable)
import Data.Int
import Data.Kind          (Type)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Lens.Labels
import Prelude            hiding (head, return, (.))

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as Text


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
--
-- /Note/: Must be UTF-8 encoded.
newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Bucket where
  show :: Bucket -> String
  show =
    Text.unpack . decodeUtf8 . unBucket


data DataTypeTy
  = CounterTy
  | GrowOnlySetTy
  | HyperLogLogTy
  | MapTy
  | SetTy


-- | A Solr index name.
newtype IndexName
  = IndexName { unIndexName :: ByteString }


-- | A Riak key.
--
-- /Note/: Must be UTF-8 encoded.
newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Key where
  show :: Key -> String
  show =
    Text.unpack . decodeUtf8 . unKey


data MapValue
  = MapValueCounter Int64
  | MapValueFlag Bool
  | MapValueMap [(ByteString, MapValue)]
  | MapValueRegister ByteString
  | MapValueSet [ByteString]
  deriving (Show)


newtype Metadata
  = Metadata { unMetadata :: [(ByteString, Maybe ByteString)] }
  deriving (Show)


data Modified a
  = Unmodified
  | Modified a


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


newtype SomeBucketType
  = SomeBucketType { unSomeBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show SomeBucketType where
  show :: SomeBucketType -> String
  show =
    Text.unpack . decodeUtf8 . unSomeBucketType


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
