{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
             LambdaCase, MagicHash, NoImplicitPrelude, OverloadedLabels,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies #-}

module Riak.Internal.DataTypes
  ( DataTypeError(..)
  , IsDataType(..)
  , IsMap(..)
  , IsRegister(..)
  , IsSet
  , MapValue(..)
  , SetOp(..)
  , setAddOp
  , setRemoveOp
  ) where

import Data.Bifunctor     (first)
import Data.DList         (DList)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Lens.Labels

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import Proto.Riak            hiding (SetOp)
import Riak.Internal.Prelude
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


-- WIP parsing primitives

data MapParseError
  = TypeMismatch
  | UnexpectedKeys
  | ParseFailure Text

data MapParser a
  = MapParser
      (HashMap ByteString MapValue -> Either MapParseError a)
      (Maybe (HashMap ByteString ()))

instance Functor MapParser where

runParser :: MapParser a -> HashMap ByteString MapValue -> Either MapParseError a
runParser (MapParser f expected) m = do
  for_ expected $ \expected' ->
    unless (null (HashMap.differenceWith (\_ _ -> Nothing) m expected'))
      (Left UnexpectedKeys)
  f m

counterField :: ByteString -> MapParser Int64
counterField key =
  MapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString MapValue -> Either MapParseError Int64
  f m =
    case HashMap.lookup key m of
      Nothing ->
        Right 0

      Just (MapValueCounter n) ->
        Right n

      Just _ ->
        Left TypeMismatch

flagField :: ByteString -> MapParser Bool
flagField key =
  MapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString MapValue -> Either MapParseError Bool
  f m =
    case HashMap.lookup key m of
      Nothing ->
        Right False

      Just (MapValueFlag b) ->
        Right b

      Just _ ->
        Left TypeMismatch

registerField :: forall a. IsRegister a => ByteString -> MapParser a
registerField key =
  MapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString MapValue -> Either MapParseError a
  f m =
    case HashMap.lookup key m of
      Nothing ->
        first ParseFailure (decodeRegister mempty)

      Just (MapValueRegister bytes) ->
        first ParseFailure (decodeRegister bytes)

      Just _ ->
        Left TypeMismatch

setField :: forall a. IsSet a => ByteString -> MapParser (Set a)
setField key =
  MapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString MapValue -> Either MapParseError (Set a)
  f m =
    case HashMap.lookup key m of
      Nothing ->
        pure mempty

      Just (MapValueSet values) ->
        undefined

      Just _ ->
        Left TypeMismatch

mapField :: forall a. IsMap a => ByteString -> MapParser a
mapField key =
  MapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString MapValue -> Either MapParseError a
  f outer =
    case HashMap.lookup key outer of
      Nothing ->
        runParser mapParser mempty

      Just (MapValueMap inner) ->
        runParser mapParser inner

      Just _ ->
        Left TypeMismatch

class IsMap a where
  mapParser :: MapParser a

instance IsMap (HashMap ByteString MapValue) where
  mapParser :: MapParser (HashMap ByteString MapValue)
  mapParser =
    MapParser Right Nothing

--------------------------------------------------------------------------------
-- Register
--------------------------------------------------------------------------------

-- TODO better name for IsRegister
class IsRegister a where
  decodeRegister :: ByteString -> Either Text a

  encodeRegister :: a -> ByteString

instance IsRegister ByteString where
  decodeRegister :: ByteString -> Either Text ByteString
  decodeRegister =
    Right

  encodeRegister :: ByteString -> ByteString
  encodeRegister =
    id

instance IsRegister Text where
  decodeRegister :: ByteString -> Either Text Text
  decodeRegister =
    first (Text.pack . displayException) . decodeUtf8'

  encodeRegister :: Text -> ByteString
  encodeRegister =
    encodeUtf8


--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

class (IsRegister a, Ord a) => IsSet a

instance IsSet a => IsDataType ('SetTy a) where
  type DataTypeVal ('SetTy a) = Set a

  parseDtFetchResp _ resp =
    case resp ^. #type' of
      DtFetchResp'SET ->
        Set.fromList <$> for (resp ^. #value . #setValue) decodeRegister

      x ->
        Left ("expected set but found " <> dataTypeToText x)

  parseDtUpdateResp _ resp =
    Set.fromList <$> for (resp ^. #setValue) decodeRegister


newtype SetOp a
  = SetOp (DList a, DList a)
  deriving newtype (Monoid, Semigroup)

setAddOp :: a -> SetOp a
setAddOp x =
  SetOp (pure x, mempty)

setRemoveOp :: a -> SetOp a
setRemoveOp x =
  SetOp (mempty, pure x)
