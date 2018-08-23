{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, GADTs,
             GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
             LambdaCase, MagicHash, OverloadedLabels, OverloadedStrings,
             StandaloneDeriving, TypeFamilies #-}

module Riak.Internal.DataTypes
  ( DataTypeError(..)
  , IsDataType(..)
  , MapValue(..)
  , IsSetContent(..)
  , SetOp(..)
  , setAddOp
  , setRemoveOp
  ) where

import Data.ByteString     (ByteString)
import Data.DList          (DList)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Set            (Set)
import Data.Text           (Text)
import Data.Traversable    (for)
import Data.Word
import GHC.Exts            (Proxy#)
import Lens.Labels
import Prelude             hiding (head, return, (.))
import UnliftIO.Exception  (Exception)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set

import Proto.Riak          hiding (SetOp)
import Riak.Internal.Types


class IsDataType (ty :: DataTypeTy) where
  type DataTypeVal    ty :: *

  parseDtFetchResp :: Proxy# ty -> DtFetchResp -> Either Text (DataTypeVal ty)

  parseDtUpdateResp :: Proxy# ty -> DtUpdateResp -> Either Text (DataTypeVal ty)


-- | A 'DataTypeError' is thrown when a data type operation is performed on an
-- incompatible bucket type (for example, attempting to fetch a counter from a
-- bucket type that contains sets).
data DataTypeError where
  DataTypeError :: Location ty -> Text -> DataTypeError

deriving instance Show DataTypeError

instance Exception DataTypeError


dataTypeToText :: DtFetchResp'DataType -> Text
dataTypeToText = \case
  DtFetchResp'COUNTER -> "counter"
  DtFetchResp'GSET    -> "gset"
  DtFetchResp'HLL     -> "hll"
  DtFetchResp'MAP     -> "map"
  DtFetchResp'SET     -> "set"


--------------------------------------------------------------------------------
-- Counter
--------------------------------------------------------------------------------

instance IsDataType 'CounterTy where
  type DataTypeVal 'CounterTy = Int64

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'COUNTER -> Right (resp ^. #value . #counterValue)
      x -> Left ("expected counter but found " <> dataTypeToText x)

  parseDtUpdateResp _ resp =
    Right (resp ^. #counterValue)


--------------------------------------------------------------------------------
-- Grow-only set
--------------------------------------------------------------------------------

instance IsDataType 'GrowOnlySetTy where
  type DataTypeVal 'GrowOnlySetTy = Set ByteString

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'GSET -> Right (Set.fromList (resp ^. #value . #gsetValue))
      x -> Left ("expected gset but found " <>dataTypeToText x)

  parseDtUpdateResp _ resp =
    Right (Set.fromList (resp ^. #gsetValue))


--------------------------------------------------------------------------------
-- HyperLogLog
--------------------------------------------------------------------------------

instance IsDataType 'HyperLogLogTy where
  type DataTypeVal 'HyperLogLogTy = Word64

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'HLL -> Right (resp ^. #value . #hllValue)
      x               -> Left ("expected hll but found " <> dataTypeToText x)

  parseDtUpdateResp _ resp =
    Right (resp ^. #hllValue)

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

instance IsDataType 'MapTy where
  type DataTypeVal 'MapTy = HashMap ByteString MapValue

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'MAP ->
        Right (parseMapEntries (resp ^. #value . #mapValue))
      x ->
        Left ("expected map but found " <> dataTypeToText x)
   where

  parseDtUpdateResp _ resp =
    Right (parseMapEntries (resp ^. #mapValue))

parseMapEntries :: [MapEntry] -> HashMap ByteString MapValue
parseMapEntries =
  foldMap $ \entry ->
    let
      MapField k ty _ =
        entry ^. #field
    in
      HashMap.singleton k $
        case ty of
          MapField'COUNTER ->
            MapValueCounter (entry ^. #counterValue)

          MapField'FLAG ->
            MapValueFlag (entry ^. #flagValue)

          MapField'MAP ->
            MapValueMap (parseMapEntries (entry ^. #mapValue))

          MapField'REGISTER ->
            MapValueRegister (entry ^. #registerValue)

          MapField'SET ->
            MapValueSet (Set.fromList (entry ^. #setValue))

data MapValue
  = MapValueCounter Int64
  | MapValueFlag Bool
  | MapValueMap (HashMap ByteString MapValue)
  | MapValueRegister ByteString
  | MapValueSet (Set ByteString)
  deriving (Show)


--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

instance IsSetContent a => IsDataType ('SetTy a) where
  type DataTypeVal ('SetTy a) = Set a

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'SET ->
        Set.fromList <$> for (resp ^. #value . #setValue) decodeSetContent

      x ->
        Left ("expected set but found " <> dataTypeToText x)

  parseDtUpdateResp _ resp =
    Set.fromList <$> for (resp ^. #setValue) decodeSetContent

-- TODO better name for IsSetContent
class Ord a => IsSetContent a where
  decodeSetContent :: ByteString -> Either Text a

  encodeSetContent :: a -> ByteString

instance IsSetContent ByteString where
  decodeSetContent = Right
  encodeSetContent = id


newtype SetOp a
  = SetOp (DList a, DList a)
  deriving newtype (Monoid, Semigroup)

setAddOp :: a -> SetOp a
setAddOp x =
  SetOp (pure x, mempty)

setRemoveOp :: a -> SetOp a
setRemoveOp x =
  SetOp (mempty, pure x)
