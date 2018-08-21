{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, GADTs,
             GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
             OverloadedStrings, PatternSynonyms #-}

-- | Sin-bin of misc. types.

module Riak.Internal.Types where

import Data.ByteString    (ByteString)
import Data.Coerce        (coerce)
import Data.Default.Class
import Data.Hashable      (Hashable)
import Data.Kind          (Type)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Lens.Labels
import Prelude            hiding (head, return, (.))
import UnliftIO.Exception (Exception)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as Text

import Proto.Riak

-- TODO Strip Param* prefix

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


data IfModified a
  = Unmodified
  | Modified a


-- | A Riak key.
newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Key where
  show :: Key -> String
  show =
    Text.unpack . decodeUtf8 . unKey


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


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False


newtype SomeBucketType
  = SomeBucketType { unSomeBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show SomeBucketType where
  show :: SomeBucketType -> String
  show =
    Text.unpack . decodeUtf8 . unSomeBucketType


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

-- | Whether or not to use the "basic quorum" policy for not-founds.
newtype ParamBasicQuorum
  = ParamBasicQuorum Bool

instance Default ParamBasicQuorum where
  def = coerce False


newtype ParamDW
  = ParamDW Quorum

instance Default ParamDW where
  def = coerce QuorumDefault


data ParamHead a :: Bool -> Type where
  ParamHead   :: ParamHead a 'True
  ParamNoHead :: ParamHead a 'False


data ParamIfModified :: Bool -> Type where
  ParamIfModified   :: ParamIfModified 'True
  ParamNoIfModified :: ParamIfModified 'False

instance (a ~ 'False) => Default (ParamIfModified a) where
  def = ParamNoIfModified


newtype ParamIncludeContext
  = ParamIncludeContext Bool

instance Default ParamIncludeContext where
  def = coerce True


-- | Whether to treat not-found responses as successful.
newtype ParamNotfoundOk
  = ParamNotfoundOk Bool

instance Default ParamNotfoundOk where
  def = coerce True


newtype ParamNVal
  = ParamNVal (Maybe Word32)

instance Default ParamNVal where
  def = ParamNVal Nothing


data ParamObjectReturn :: ObjectReturn -> Type where
  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody


newtype ParamPR
  = ParamPR Quorum

instance Default ParamPR where
  def = coerce QuorumDefault


newtype ParamPW
  = ParamPW Quorum

instance Default ParamPW where
  def = coerce QuorumDefault


newtype ParamR
  = ParamR Quorum

instance Default ParamR where
  def = coerce QuorumDefault


-- TODO remove ParamReturnBody
newtype ParamReturnBody
  = ParamReturnBody Bool

instance Default ParamReturnBody where
  def = coerce False


-- TODO remove ParamReturnHead
newtype ParamReturnHead
  = ParamReturnHead Bool

instance Default ParamReturnHead where
  def = coerce False


newtype ParamSloppyQuorum
  = ParamSloppyQuorum Bool

instance Default ParamSloppyQuorum where
  def = coerce False


newtype ParamTimeout
  = ParamTimeout (Maybe Word32)

instance Default ParamTimeout where
  def = ParamTimeout Nothing


newtype ParamW
  = ParamW Quorum

instance Default ParamW where
  def = coerce QuorumDefault
