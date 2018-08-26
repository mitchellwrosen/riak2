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
  , RiakMapFieldParser
  , riakCounterField
  , riakFlagField
  , riakMapField
  , riakRegisterField
  , riakSetField
  , allowExtraKeys
  , RiakMapEntries(..)
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
          (runFieldParser
            decodeRiakMap
            (parseMapEntries' (resp ^. #value . #mapValue)))

      x ->
        (Left . toException . RiakDataTypeError loc)
          ("expected map but found " <> dataTypeToText x)

  parseDtUpdateResp
    :: RiakLocation ('Just ('RiakMapTy a))
    -> DtUpdateResp
    -> Either SomeException a
  parseDtUpdateResp _ resp =
    first toException
      (runFieldParser
        decodeRiakMap
        (parseMapEntries' (resp ^. #mapValue)))

-- TODO Fix this, ByteString keys are not necessarily unique
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

parseMapEntries' :: [MapEntry] -> RiakMapEntries
parseMapEntries' =
  foldMap $ \entry ->
    let
      MapField k ty _ =
        entry ^. #field
    in
      case ty of
        MapField'COUNTER ->
          RiakMapEntries
            (HashMap.singleton k (entry ^. #counterValue))
            mempty
            mempty
            mempty
            mempty

        MapField'FLAG ->
          RiakMapEntries
            mempty
            (HashMap.singleton k (entry ^. #flagValue))
            mempty
            mempty
            mempty

        MapField'MAP ->
          RiakMapEntries
            mempty
            mempty
            (HashMap.singleton k (parseMapEntries' (entry ^. #mapValue)))
            mempty
            mempty

        MapField'REGISTER ->
          RiakMapEntries
            mempty
            mempty
            mempty
            (HashMap.singleton k (entry ^. #registerValue))
            mempty

        MapField'SET ->
          RiakMapEntries
            mempty
            mempty
            mempty
            mempty
            (HashMap.singleton k (Set.fromList (entry ^. #setValue)))

data RiakMapEntries
  = RiakMapEntries
      !(HashMap ByteString Int64)
      !(HashMap ByteString Bool)
      !(HashMap ByteString RiakMapEntries)
      !(HashMap ByteString ByteString)
      !(HashMap ByteString (Set ByteString))
  deriving (Show)

instance Monoid RiakMapEntries where
  mempty = RiakMapEntries mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup RiakMapEntries where
  RiakMapEntries a1 b1 c1 d1 e1 <> RiakMapEntries a2 b2 c2 d2 e2 =
    RiakMapEntries (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

data RiakMapValue
  = RiakMapValueCounter Int64
  | RiakMapValueFlag Bool
  | RiakMapValueMap (HashMap ByteString RiakMapValue)
  | RiakMapValueRegister ByteString
  | RiakMapValueSet (Set ByteString)
  deriving (Show)

-- Should return same strings as dataTypeToText
riakMapValueToText :: RiakMapValue -> Text
riakMapValueToText = \case
  RiakMapValueCounter  _ -> "counter"
  RiakMapValueFlag     _ -> "flag"
  RiakMapValueMap      _ -> "map"
  RiakMapValueRegister _ -> "register"
  RiakMapValueSet      _ -> "set"


-- | An error occurred when decoding a Riak map.
data RiakMapParseError
  = TypeMismatch Text Text
  | UnexpectedKeys [ByteString]
  | ParseFailure SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data RiakMapFieldParser a
  = RiakMapFieldParser
      (RiakMapEntries -> Either RiakMapParseError a)
      (Maybe (HashMap ByteString ()))
  deriving (Functor)

runFieldParser
  :: RiakMapFieldParser a
  -> RiakMapEntries
  -> Either RiakMapParseError a
runFieldParser (RiakMapFieldParser f expected) m =
  undefined
  -- for_ expected $ \expected' ->
  --   let
  --     unexpected =
  --       HashMap.differenceWith (\_ _ -> Nothing) m expected'
  --   in
  --     unless (null unexpected)
  --       (Left (UnexpectedKeys (HashMap.keys unexpected)))
  -- f m

allowExtraKeys :: RiakMapFieldParser a -> RiakMapFieldParser a
allowExtraKeys (RiakMapFieldParser f _) =
  RiakMapFieldParser f Nothing

riakCounterField :: ByteString -> RiakMapFieldParser Int64
riakCounterField key =
  RiakMapFieldParser f (Just (HashMap.singleton key ()))
 where
  f :: RiakMapEntries -> Either RiakMapParseError Int64
  f m =
    undefined
    -- case HashMap.lookup key m of
    --   Nothing ->
    --     Right 0

    --   Just (RiakMapValueCounter n) ->
    --     Right n

    --   Just x ->
    --     Left (TypeMismatch (riakMapValueToText x) "counter")

riakFlagField :: ByteString -> RiakMapFieldParser Bool
riakFlagField key =
  RiakMapFieldParser f (Just (HashMap.singleton key ()))
 where
  f :: RiakMapEntries -> Either RiakMapParseError Bool
  f m =
    undefined
    -- case HashMap.lookup key m of
    --   Nothing ->
    --     Right False

    --   Just (RiakMapValueFlag b) ->
    --     Right b

    --   Just x ->
    --     Left (TypeMismatch (riakMapValueToText x) "flag")

riakRegisterField :: forall a. IsRiakRegister a => ByteString -> RiakMapFieldParser a
riakRegisterField key =
  RiakMapFieldParser f (Just (HashMap.singleton key ()))
 where
  f :: RiakMapEntries -> Either RiakMapParseError a
  f m =
    undefined
    -- case HashMap.lookup key m of
    --   Nothing ->
    --     first ParseFailure (decodeRiakRegister mempty)

    --   Just (RiakMapValueRegister bytes) ->
    --     first ParseFailure (decodeRiakRegister bytes)

    --   Just x ->
    --     Left (TypeMismatch (riakMapValueToText x) "register")

riakSetField :: forall a. IsRiakSet a => ByteString -> RiakMapFieldParser (Set a)
riakSetField key =
  RiakMapFieldParser f (Just (HashMap.singleton key ()))
 where
  f :: RiakMapEntries -> Either RiakMapParseError (Set a)
  f m =
    undefined
    -- case HashMap.lookup key m of
    --   Nothing ->
    --     pure mempty

    --   Just (RiakMapValueSet values) ->
    --     -- TODO make it so I don't have to pay for to/from/to set
    --     bimap ParseFailure (Set.fromList)
    --       (traverse decodeRiakRegister (toList values))

    --   Just x ->
    --     Left (TypeMismatch (riakMapValueToText x) "set")

riakMapField :: forall a. IsRiakMap a => ByteString -> RiakMapFieldParser a
riakMapField key =
  RiakMapFieldParser f (Just (HashMap.singleton key ()))
 where
  f :: RiakMapEntries -> Either RiakMapParseError a
  f outer =
    undefined
    -- case HashMap.lookup key outer of
    --   Nothing ->
    --     runFieldParser decodeRiakMap mempty

    --   Just (RiakMapValueMap inner) ->
    --     runFieldParser decodeRiakMap inner

    --   Just x ->
    --     Left (TypeMismatch (riakMapValueToText x) "map")

-- | 'IsRiakMap' classifies types that are stored in Riak map data types. A Riak
-- map contains any number of key-value pairs, where the keys are byte arrays,
-- and the values are either counters, flags, registers, sets, or maps.
--
-- This data structure can be parsed in three ways:
--
-- [Untyped]
-- An @untyped@ map is simply the primitive key-value pairs stored in Riak. It
-- is provided by the instance @IsRiakMap RiakMapEntries@.
--
-- [Homogenous]
-- TODO
--
-- [Heterogeneous]
-- A @heterogeneous@ map is an arbitrary data structure encoded as a Riak map.
-- Its decoder is provided by a user-supplied 'decodeRiakMap', which is built
-- from individual field parsers such as 'riakCounterField'.
--
-- By default, when parsing a heterogeneous map, any unexpected keys will be
-- treated as an error. For more lenient parsing, use 'allowExtraKeys'.
class IsRiakMap a where
  decodeRiakMap :: RiakMapFieldParser a

instance IsRiakMap RiakMapEntries where
  decodeRiakMap :: RiakMapFieldParser RiakMapEntries
  decodeRiakMap =
    RiakMapFieldParser Right Nothing

--------------------------------------------------------------------------------
-- Register
--------------------------------------------------------------------------------

-- | 'IsRiakRegister' classifies types that are stored in Riak register data
-- types, which are simple arrays of bytes with a timestamp-based,
-- last-write-wins conflict resolution strategy.
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

-- | 'IsRiakSet' classifies types that are stored in Riak set data types. Riak
-- sets only contain arrays of bytes, like Riak registers, so this type class is
-- satisfied for all types that are instances of both 'IsRiakRegister' and
-- 'Ord'.
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
