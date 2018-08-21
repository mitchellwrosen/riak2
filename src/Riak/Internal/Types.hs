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
import UnliftIO.Exception (Exception)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as Text

import Proto.Riak


-- | Whether or not to use the "basic quorum" policy for not-founds.
newtype BasicQuorum
  = BasicQuorum Bool

instance Default BasicQuorum where
  def = coerce False


-- | A Riak bucket type, tagged with the data type it contains.
newtype BucketType (ty :: Maybe DataType)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

pattern BucketTypeDefault :: BucketType 'Nothing
pattern BucketTypeDefault =
  BucketType "default"


-- | A Riak bucket.
newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Bucket where
  show :: Bucket -> String
  show =
    Text.unpack . decodeUtf8 . unBucket


data DataType
  = DataTypeCounter
  | DataTypeMap
  | DataTypeSet
  -- TODO hll, gset


-- | A 'DataTypeError' is thrown when a data type operation is performed on an
-- incompatible bucket type (for example, attempting to fetch a counter from a
-- bucket type that contains sets).
data DataTypeError
  = DataTypeError
      !SomeBucketType       -- Bucket type
      !Bucket               -- Bucket
      !Key                  -- Key
      !DtFetchResp'DataType -- Actual data type
      !DtFetchResp'DataType -- Expected data type
  deriving stock (Show)
  deriving anyclass (Exception)


newtype DW
  = DW Quorum

instance Default DW where
  def = coerce QuorumDefault


data Index
  = IndexInt !ByteString !Int64
  | IndexBin !ByteString !ByteString
  deriving (Show)


newtype Indexes
  = Indexes { unIndexes :: [Index] }
  deriving (Show)


-- | A Riak key.
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


-- | Whether to treat not-found responses as successful.
newtype NotfoundOk
  = NotfoundOk Bool

instance Default NotfoundOk where
  def = coerce True


newtype Nval
  = Nval (Maybe Word32)

instance Default Nval where
  def = Nval Nothing


data ObjectReturn
  = ObjectReturnNone
  | ObjectReturnHead
  | ObjectReturnBody

-- TODO singletons
data SObjectReturn :: ObjectReturn -> Type where
  SObjectReturnNone :: SObjectReturn 'ObjectReturnNone
  SObjectReturnHead :: SObjectReturn 'ObjectReturnHead
  SObjectReturnBody :: SObjectReturn 'ObjectReturnBody

class SingObjectReturn (a :: ObjectReturn) where
  singObjectReturn :: SObjectReturn a

instance SingObjectReturn 'ObjectReturnNone where singObjectReturn = SObjectReturnNone
instance SingObjectReturn 'ObjectReturnHead where singObjectReturn = SObjectReturnHead
instance SingObjectReturn 'ObjectReturnBody where singObjectReturn = SObjectReturnBody


newtype Quorum
  = Quorum Word32
  deriving stock (Eq)
  deriving newtype (Num)

pattern QuorumAll :: Quorum
pattern QuorumAll = 4294967292

pattern QuorumDefault :: Quorum
pattern QuorumDefault = 4294967291

pattern QuorumOne :: Quorum
pattern QuorumOne = 4294967294

pattern QuorumQuorum :: Quorum
pattern QuorumQuorum = 4294967293


newtype PR
  = PR Quorum

instance Default PR where
  def = coerce QuorumDefault


newtype PW
  = PW Quorum

instance Default PW where
  def = coerce QuorumDefault


newtype R
  = R Quorum

instance Default R where
  def = coerce QuorumDefault


-- TODO remove ReturnBody
newtype ReturnBody
  = ReturnBody Bool

instance Default ReturnBody where
  def = coerce False


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False


newtype SloppyQuorum
  = SloppyQuorum Bool

instance Default SloppyQuorum where
  def = coerce False


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


newtype W
  = W Quorum

instance Default W where
  def = coerce QuorumDefault


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
