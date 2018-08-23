{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, KindSignatures,
             LambdaCase, MagicHash, OverloadedLabels, OverloadedStrings,
             TypeFamilies #-}

module Riak.Internal.DataTypes
  ( DataType(..)
  , DataTypeError(..)
  , DataTypeVal
  , IsDataType(..)
  , MapValue(..)
  ) where

import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Set            (Set)
import Data.Text           (Text)
import Data.Word
import GHC.Exts            (Proxy#)
import Lens.Labels
import Prelude             hiding (head, return, (.))
import UnliftIO.Exception  (Exception)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set

import Proto.Riak
import Riak.Internal.Types


type family DataTypeVal (ty :: DataTypeTy) where
  DataTypeVal 'CounterTy     = Int64
  DataTypeVal 'GrowOnlySetTy = Set ByteString
  DataTypeVal 'HyperLogLogTy = Word64
  DataTypeVal 'MapTy         = HashMap ByteString MapValue
  DataTypeVal 'SetTy         = Set ByteString


class IsDataType (ty :: DataTypeTy) where
  fetchRespTy  :: Proxy# ty -> DtFetchResp'DataType

  fromDataType :: Proxy# ty -> DataType -> Either Text (DataTypeVal ty)

  toDataType   :: Proxy# ty -> DtValue -> DataType

instance IsDataType 'CounterTy where
  fetchRespTy _ =
    DtFetchResp'COUNTER

  fromDataType _ = \case
    DataTypeCounter n -> Right n
    x -> Left ("expected counter but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeCounter . view #counterValue

instance IsDataType 'GrowOnlySetTy where
  fetchRespTy _ =
    DtFetchResp'GSET

  fromDataType _ = \case
    DataTypeGrowOnlySet n -> Right n
    x -> Left ("expected gset but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeGrowOnlySet . Set.fromList . view #gsetValue

instance IsDataType 'HyperLogLogTy where
  fetchRespTy _ =
    DtFetchResp'HLL

  fromDataType _ = \case
    DataTypeHyperLogLog n -> Right n
    x -> Left ("expected hll but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeHyperLogLog . view #hllValue

instance IsDataType 'MapTy where
  fetchRespTy _ =
    DtFetchResp'MAP

  fromDataType _ = \case
    DataTypeMap n -> Right n
    x -> Left ("expected map but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeMap . foldMap mapEntryToPair . view #mapValue
   where
    mapEntryToPair :: MapEntry -> HashMap ByteString MapValue
    mapEntryToPair entry =
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
              MapValueMap (foldMap mapEntryToPair (entry ^. #mapValue))

            MapField'REGISTER ->
              MapValueRegister (entry ^. #registerValue)

            MapField'SET ->
              MapValueSet (Set.fromList (entry ^. #setValue))

instance IsDataType 'SetTy where
  fetchRespTy _ =
    DtFetchResp'SET

  fromDataType _ = \case
    DataTypeSet n -> Right n
    x -> Left ("expected set but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeSet . Set.fromList . view #setValue


data DataType
  = DataTypeCounter Int64
  | DataTypeGrowOnlySet (Set ByteString)
  | DataTypeHyperLogLog Word64
  | DataTypeMap (HashMap ByteString MapValue)
  | DataTypeSet (Set ByteString)

dataTypeToText :: DataType -> Text
dataTypeToText = \case
  DataTypeCounter     _ -> "counter"
  DataTypeGrowOnlySet _ -> "gset"
  DataTypeHyperLogLog _ -> "hll"
  DataTypeMap         _ -> "map"
  DataTypeSet         _ -> "set"


data MapValue
  = MapValueCounter Int64
  | MapValueFlag Bool
  | MapValueMap (HashMap ByteString MapValue)
  | MapValueRegister ByteString
  | MapValueSet (Set ByteString)
  deriving (Show)


-- | A 'DataTypeError' is thrown when a data type operation is performed on an
-- incompatible bucket type (for example, attempting to fetch a counter from a
-- bucket type that contains sets).
data DataTypeError
  = DataTypeError
      !SomeBucketType       -- Bucket type
      !Bucket               -- Bucket
      !Key                  -- Key
      !Text                 -- Error message
  deriving stock (Show)
  deriving anyclass (Exception)
