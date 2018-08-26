{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DerivingStrategies,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             InstanceSigs, KindSignatures, LambdaCase, MagicHash,
             NoImplicitPrelude, OverloadedLabels, OverloadedStrings,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies,
             UndecidableInstances #-}

module Riak.Internal.DataTypes
  ( RiakDataTypeError(..)
  , IsRiakDataType(..)
  , IsRiakMap(..)
  , IsRiakRegister(..)
  , IsRiakSet
  , RiakMapParser
  , riakCounterField
  , riakFlagField
  , riakMapField
  , riakRegisterField
  , riakSetField
  , allowExtraKeys
  , RiakMapValue(..)
  , RiakSetOp(..)
  , riakSetAddOp
  , riakSetRemoveOp
  ) where

import Data.Bifunctor     (first)
import Data.DList         (DList)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Lens.Labels

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set

import Proto.Riak
import Riak.Internal.Prelude
import Riak.Internal.Types


class IsRiakDataType (ty :: RiakDataTypeTy) where
  type DataTypeVal    ty :: *

  parseDtFetchResp
    :: RiakLocation ('Just ty)
    -> DtFetchResp
    -> Either SomeException (DataTypeVal ty)

  parseDtUpdateResp
    :: RiakLocation ('Just ty)
    -> DtUpdateResp
    -> Either SomeException (DataTypeVal ty)


-- | A 'RiakDataTypeError' is thrown when a data type operation is performed on
-- an incompatible bucket type (for example, attempting to fetch a counter from
-- a bucket type that contains sets).
data RiakDataTypeError where
  RiakDataTypeError :: RiakLocation ty -> Text -> RiakDataTypeError

deriving instance Show RiakDataTypeError

instance Exception RiakDataTypeError


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

instance IsRiakDataType 'RiakCounterTy where
  type DataTypeVal 'RiakCounterTy = Int64

  parseDtFetchResp
    :: RiakLocation ('Just 'RiakCounterTy)
    -> DtFetchResp
    -> Either SomeException Int64
  parseDtFetchResp loc resp =
    case resp ^. #type' of
      DtFetchResp'COUNTER ->
        Right (resp ^. #value . #counterValue)

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected counter but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just 'RiakCounterTy)
    -> DtUpdateResp
    -> Either SomeException Int64
  parseDtUpdateResp _ resp =
    Right (resp ^. #counterValue)


--------------------------------------------------------------------------------
-- Grow-only set
--------------------------------------------------------------------------------

instance IsRiakDataType 'RiakGrowOnlySetTy where
  type DataTypeVal 'RiakGrowOnlySetTy = Set ByteString

  parseDtFetchResp
    :: RiakLocation ('Just 'RiakGrowOnlySetTy)
    -> DtFetchResp
    -> Either SomeException (Set ByteString)
  parseDtFetchResp loc resp =
    case resp ^. #type' of
      DtFetchResp'GSET ->
        Right (Set.fromList (resp ^. #value . #gsetValue))

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected gset but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just 'RiakGrowOnlySetTy)
    -> DtUpdateResp
    -> Either SomeException (Set ByteString)
  parseDtUpdateResp _ resp =
    Right (Set.fromList (resp ^. #gsetValue))


--------------------------------------------------------------------------------
-- HyperLogLog
--------------------------------------------------------------------------------

instance IsRiakDataType 'RiakHyperLogLogTy where
  type DataTypeVal 'RiakHyperLogLogTy = Word64

  parseDtFetchResp
    :: RiakLocation ('Just 'RiakHyperLogLogTy)
    -> DtFetchResp
    -> Either SomeException Word64
  parseDtFetchResp loc resp =
    case resp ^. #type' of
      DtFetchResp'HLL ->
        Right (resp ^. #value . #hllValue)

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected hll but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just 'RiakHyperLogLogTy)
    -> DtUpdateResp
    -> Either SomeException Word64
  parseDtUpdateResp _ resp =
    Right (resp ^. #hllValue)


--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

instance IsRiakMap a => IsRiakDataType ('RiakMapTy a) where
  type DataTypeVal ('RiakMapTy a) = a

  parseDtFetchResp
    :: RiakLocation ('Just ('RiakMapTy a))
    -> DtFetchResp
    -> Either SomeException a
  parseDtFetchResp loc resp =
    case resp ^. #type' of
      DtFetchResp'MAP ->
        first toException
          (runParser
            decodeRiakMap
            (parseMapEntries (resp ^. #value . #mapValue)))

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected map but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just ('RiakMapTy a))
    -> DtUpdateResp
    -> Either SomeException a
  parseDtUpdateResp _ resp =
    first toException
      (runParser
        decodeRiakMap
        (parseMapEntries (resp ^. #mapValue)))

parseMapEntries :: [MapEntry] -> HashMap ByteString RiakMapValue
parseMapEntries =
  foldMap $ \entry ->
    let
      MapField k ty _ =
        entry ^. #field
    in
      HashMap.singleton k $
        case ty of
          MapField'COUNTER ->
            RiakMapValueCounter (entry ^. #counterValue)

          MapField'FLAG ->
            RiakMapValueFlag (entry ^. #flagValue)

          MapField'MAP ->
            RiakMapValueMap (parseMapEntries (entry ^. #mapValue))

          MapField'REGISTER ->
            RiakMapValueRegister (entry ^. #registerValue)

          MapField'SET ->
            RiakMapValueSet (Set.fromList (entry ^. #setValue))


data RiakMapValue
  = RiakMapValueCounter Int64
  | RiakMapValueFlag Bool
  | RiakMapValueMap (HashMap ByteString RiakMapValue)
  | RiakMapValueRegister ByteString
  | RiakMapValueSet (Set ByteString)
  deriving (Show)


data RiakMapParseError
  = TypeMismatch -- TODO more informative TypeMismatch
  | UnexpectedKeys -- TODO more informative UnexpectedKeys
  | ParseFailure SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data RiakMapParser a
  = RiakMapParser
      (HashMap ByteString RiakMapValue -> Either RiakMapParseError a)
      (Maybe (HashMap ByteString ()))
  deriving (Functor)

runParser
  :: RiakMapParser a
  -> HashMap ByteString RiakMapValue
  -> Either RiakMapParseError a
runParser (RiakMapParser f expected) m = do
  for_ expected $ \expected' ->
    unless (null (HashMap.differenceWith (\_ _ -> Nothing) m expected'))
      (Left UnexpectedKeys)
  f m

allowExtraKeys :: RiakMapParser a -> RiakMapParser a
allowExtraKeys (RiakMapParser f _) =
  RiakMapParser f Nothing

riakCounterField :: ByteString -> RiakMapParser Int64
riakCounterField key =
  RiakMapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString RiakMapValue -> Either RiakMapParseError Int64
  f m =
    case HashMap.lookup key m of
      Nothing ->
        Right 0

      Just (RiakMapValueCounter n) ->
        Right n

      Just _ ->
        Left TypeMismatch

riakFlagField :: ByteString -> RiakMapParser Bool
riakFlagField key =
  RiakMapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString RiakMapValue -> Either RiakMapParseError Bool
  f m =
    case HashMap.lookup key m of
      Nothing ->
        Right False

      Just (RiakMapValueFlag b) ->
        Right b

      Just _ ->
        Left TypeMismatch

riakRegisterField :: forall a. IsRiakRegister a => ByteString -> RiakMapParser a
riakRegisterField key =
  RiakMapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString RiakMapValue -> Either RiakMapParseError a
  f m =
    case HashMap.lookup key m of
      Nothing ->
        first ParseFailure (decodeRiakRegister mempty)

      Just (RiakMapValueRegister bytes) ->
        first ParseFailure (decodeRiakRegister bytes)

      Just _ ->
        Left TypeMismatch

riakSetField :: forall a. IsRiakSet a => ByteString -> RiakMapParser (Set a)
riakSetField key =
  RiakMapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString RiakMapValue -> Either RiakMapParseError (Set a)
  f m =
    case HashMap.lookup key m of
      Nothing ->
        pure mempty

      Just (RiakMapValueSet values) ->
        -- TODO make it so I don't have to pay for to/from/to set
        bimap ParseFailure (Set.fromList)
          (traverse decodeRiakRegister (toList values))

      Just _ ->
        Left TypeMismatch

riakMapField :: forall a. IsRiakMap a => ByteString -> RiakMapParser a
riakMapField key =
  RiakMapParser f (Just (HashMap.singleton key ()))
 where
  f :: HashMap ByteString RiakMapValue -> Either RiakMapParseError a
  f outer =
    case HashMap.lookup key outer of
      Nothing ->
        runParser decodeRiakMap mempty

      Just (RiakMapValueMap inner) ->
        runParser decodeRiakMap inner

      Just _ ->
        Left TypeMismatch

class IsRiakMap a where
  decodeRiakMap :: RiakMapParser a

instance IsRiakMap (HashMap ByteString RiakMapValue) where
  decodeRiakMap :: RiakMapParser (HashMap ByteString RiakMapValue)
  decodeRiakMap =
    RiakMapParser Right Nothing

--------------------------------------------------------------------------------
-- Register
--------------------------------------------------------------------------------

class IsRiakRegister a where
  decodeRiakRegister :: ByteString -> Either SomeException a

  encodeRiakRegister :: a -> ByteString

instance IsRiakRegister ByteString where
  decodeRiakRegister :: ByteString -> Either SomeException ByteString
  decodeRiakRegister =
    Right

  encodeRiakRegister :: ByteString -> ByteString
  encodeRiakRegister =
    id

instance IsRiakRegister Text where
  decodeRiakRegister :: ByteString -> Either SomeException Text
  decodeRiakRegister =
    first toException . decodeUtf8'

  encodeRiakRegister :: Text -> ByteString
  encodeRiakRegister =
    encodeUtf8


--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

class (IsRiakRegister a, Ord a) => IsRiakSet a
instance (IsRiakRegister a, Ord a) => IsRiakSet a

instance IsRiakSet a => IsRiakDataType ('RiakSetTy a) where
  type DataTypeVal ('RiakSetTy a) = Set a

  parseDtFetchResp
    :: RiakLocation ('Just ('RiakSetTy a))
    -> DtFetchResp
    -> Either SomeException (Set a)
  parseDtFetchResp loc resp =
    case resp ^. #type' of
      DtFetchResp'SET ->
        Set.fromList <$> for (resp ^. #value . #setValue) decodeRiakRegister

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected set but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just ('RiakSetTy a))
    -> DtUpdateResp
    -> Either SomeException (Set a)
  parseDtUpdateResp _ resp =
    Set.fromList <$> for (resp ^. #setValue) decodeRiakRegister


newtype RiakSetOp a
  = RiakSetOp (DList a, DList a)
  deriving newtype (Monoid, Semigroup)

riakSetAddOp :: a -> RiakSetOp a
riakSetAddOp x =
  RiakSetOp (pure x, mempty)

riakSetRemoveOp :: a -> RiakSetOp a
riakSetRemoveOp x =
  RiakSetOp (mempty, pure x)
