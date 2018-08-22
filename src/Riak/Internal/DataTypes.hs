{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, KindSignatures,
             LambdaCase, MagicHash, OverloadedLabels, OverloadedStrings,
             TupleSections, TypeFamilies #-}

module Riak.Internal.DataTypes
  ( DataType(..)
  , DataTypeError(..)
  , DataTypeVal
  , IsDataType(..)
  ) where

import Data.ByteString    (ByteString)
import Data.Int
import Data.Text          (Text)
import Data.Word
import GHC.Exts           (Proxy#)
import Lens.Labels
import Prelude            hiding (head, return, (.))
import UnliftIO.Exception (Exception)

import Proto.Riak
import Riak.Internal.Types


type family DataTypeVal (ty :: DataTypeTy) where
  DataTypeVal 'DataTypeCounterTy     = Int64
  DataTypeVal 'DataTypeGrowOnlySetTy = [ByteString]
  DataTypeVal 'DataTypeHyperLogLogTy = Word64
  DataTypeVal 'DataTypeMapTy         = [(ByteString, MapValue)]
  DataTypeVal 'DataTypeSetTy         = [ByteString]


class IsDataType (ty :: DataTypeTy) where
  fetchRespTy  :: Proxy# ty -> DtFetchResp'DataType

  fromDataType :: Proxy# ty -> DataType -> Either Text (DataTypeVal ty)

  toDataType   :: Proxy# ty -> DtValue -> DataType

instance IsDataType 'DataTypeCounterTy where
  fetchRespTy _ =
    DtFetchResp'COUNTER

  fromDataType _ = \case
    DataTypeCounter n -> Right n
    x -> Left ("expected counter but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeCounter . view #counterValue

instance IsDataType 'DataTypeGrowOnlySetTy where
  fetchRespTy _ =
    DtFetchResp'GSET

  fromDataType _ = \case
    DataTypeGrowOnlySet n -> Right n
    x -> Left ("expected gset but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeGrowOnlySet . view #gsetValue

instance IsDataType 'DataTypeHyperLogLogTy where
  fetchRespTy _ =
    DtFetchResp'HLL

  fromDataType _ = \case
    DataTypeHyperLogLog n -> Right n
    x -> Left ("expected hll but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeHyperLogLog . view #hllValue

instance IsDataType 'DataTypeMapTy where
  fetchRespTy _ =
    DtFetchResp'MAP

  fromDataType _ = \case
    DataTypeMap n -> Right n
    x -> Left ("expected map but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeMap . map mapEntryToPair . view #mapValue
   where
    mapEntryToPair :: MapEntry -> (ByteString, MapValue)
    mapEntryToPair entry =
      let
        MapField k ty _ =
          entry ^. #field
      in
        (k ,) $
          case ty of
            MapField'COUNTER ->
              MapValueCounter (entry ^. #counterValue)

            MapField'FLAG ->
              MapValueFlag (entry ^. #flagValue)

            MapField'MAP ->
              MapValueMap (map mapEntryToPair (entry ^. #mapValue))

            MapField'REGISTER ->
              MapValueRegister (entry ^. #registerValue)

            MapField'SET ->
              MapValueSet (entry ^. #setValue)

instance IsDataType 'DataTypeSetTy where
  fetchRespTy _ =
    DtFetchResp'SET

  fromDataType _ = \case
    DataTypeSet n -> Right n
    x -> Left ("expected set but found " <> dataTypeToText x)

  toDataType _ =
    DataTypeSet . view #setValue


data DataType
  = DataTypeCounter Int64
  | DataTypeGrowOnlySet [ByteString]
  | DataTypeHyperLogLog Word64
  | DataTypeMap [(ByteString, MapValue)]
  | DataTypeSet [ByteString]

dataTypeToText :: DataType -> Text
dataTypeToText = \case
  DataTypeCounter     _ -> "counter"
  DataTypeGrowOnlySet _ -> "gset"
  DataTypeHyperLogLog _ -> "hll"
  DataTypeMap         _ -> "map"
  DataTypeSet         _ -> "set"


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
