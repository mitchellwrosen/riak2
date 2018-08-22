{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, GADTs,
             GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
             OverloadedStrings, PatternSynonyms #-}

-- | Sin-bin of misc. types.

module Riak.Internal.Types where

import Data.ByteString    (ByteString)
import Data.Coerce        (coerce)
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


-- | Whether to use the "basic quorum" policy for not-founds. Only relevant when
-- @notfound_ok@ is set to false.
--
-- /Default/: false.
newtype BasicQuorum
  = BasicQuorum Bool

instance Default BasicQuorum where
  def = coerce False


-- | A Riak bucket type, tagged with the data type it contains.
--
-- /Note/: Must be UTF-8 encoded.
newtype BucketType (ty :: Maybe DataTypeTy)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

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


-- | The number of vnodes that must write a write request to storage before a
-- response is returned to the client. The request will still be replicated to
-- @N@ vnodes.
--
-- /Default/: @quorum@.
-- /Range/: 1 to @N@.
newtype DW
  = DW Quorum
  deriving newtype (Default)


-- | A Solr index name.
newtype IndexName
  = IndexName { unIndex :: ByteString }


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


-- | @notfound_ok@ controls how Riak behaves during read requests when keys are
-- not present.
--
-- * If @true@, Riak will treat any @notfound@ as a positive assertion that the
-- key does not exist.
--
-- * If @false@, Riak will treat any @notfound@ as a failure condition. The
-- coordinating node will wait for enough vnodes to reply with @notfound@ to
-- know that it cannot satisfy the requested @R@.
--
-- /Default/: true.
newtype NotfoundOk
  = NotfoundOk Bool

instance Default NotfoundOk where
  def = coerce True


-- | The number of __primary vnodes__ responsible for each key, i.e. the number
-- of __replicas__ stores in the cluster.
--
-- /Default/: 3.
-- /Range/: 1 to the number of nodes in the cluster.
newtype N
  = N (Maybe Word32)

instance Default N where
  def = N Nothing


data ObjectReturn
  = ObjectReturnNone
  | ObjectReturnHead
  | ObjectReturnBody


-- | How many vnodes must respond before an operation is considered successful.
-- May be a number @<= N@, or a symbolic value.
newtype Quorum
  = Quorum Word32
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


-- | The number of primary vnodes that must respond to a read request before a
-- response is returned to the client. The request will still be replicated to
-- @N@ vnodes.
--
-- /Default/: 0.
-- /Range/: 1 to @N@.
newtype PR
  = PR Quorum
  deriving newtype (Default)


-- | The number of primary vnodes that must /respond/ to a write request before
-- a response is returned to the client. The request will still be replicated to
-- @N@ vnodes.
--
-- /Default/: 0.
-- /Range/: 1 to @N@.
newtype PW
  = PW Quorum
  deriving newtype (Default)


-- | The number of vnodes that must respond to a read request before a response
-- is returned to the client. The request will still be replicated to @N@
-- vnodes.
--
-- /Default/: @quorum@.
-- /Range/: 1 to @N@.
newtype R
  = R Quorum
  deriving newtype (Default)


-- TODO remove ReturnBody
newtype ReturnBody
  = ReturnBody Bool

instance Default ReturnBody where
  def = coerce False


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


-- | Whether failover vnodes are consulted if one or more primary vnodes fails.
--
-- /Default/: true.
newtype SloppyQuorum
  = SloppyQuorum Bool

instance Default SloppyQuorum where
  def = coerce True


newtype SomeBucketType
  = SomeBucketType { unSomeBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show SomeBucketType where
  show :: SomeBucketType -> String
  show =
    Text.unpack . decodeUtf8 . unSomeBucketType


newtype Timeout
  = Timeout { unTimeout :: Maybe Word32 }

instance Default Timeout where
  def = Timeout Nothing


newtype TTL
  = TTL { unTTL :: Maybe Word32 }
  deriving Show

instance Default TTL where
  def = TTL Nothing


newtype Vclock
  = Vclock { unVclock :: ByteString }

instance Show Vclock where
  show :: Vclock -> String
  show =
    show . Base64.encode . unVclock


newtype Vtag
  = Vtag { unVtag :: ByteString }
  deriving (Show)


-- | The number of vnodes that must /respond/ to a write request before a
-- response is returned to the client. The request will still be replicated to
-- @N@ vnodes.
--
-- /Default/: @quorum@.
-- /Range/: 1 to @N@.
newtype W
  = W Quorum
  deriving newtype (Default)


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


newtype ParamIncludeContext
  = ParamIncludeContext Bool

instance Default ParamIncludeContext where
  def = coerce True


data ParamObjectReturn :: ObjectReturn -> Type where
  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody
