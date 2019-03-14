module RiakSecondaryIndex where

import RiakPanic
import RiakUtils

import qualified RiakProtoPair as Pair

import Control.Lens    ((.~))
import Data.Riak.Proto (RpbPair)

import qualified Data.ByteString as ByteString
import qualified Data.Riak.Proto as Proto


-- | A secondary index.
data SecondaryIndex :: Type where
  BinaryIndex :: ByteString -> ByteString -> SecondaryIndex
  IntIndex :: ByteString -> Int64 -> SecondaryIndex
  deriving stock (Eq, Show)

fromPair :: RpbPair -> SecondaryIndex
fromPair =
  Pair.toTuple >>> \case
    (ByteString.stripSuffix "_bin" -> Just key, value) ->
      BinaryIndex key value

    (ByteString.stripSuffix "_int" -> Just key, value) ->
      IntIndex key (bs2int value)

    (k, v) ->
      impurePanic "RiakSecondaryIndex.fromPair"
        ( ("key",   k)
        , ("value", v)
        )

toPair :: SecondaryIndex -> RpbPair
toPair = \case
  BinaryIndex key value ->
    Proto.defMessage
      & Proto.key .~ (key <> "_bin")
      & Proto.value .~ value

  IntIndex key value ->
    Proto.defMessage
      & Proto.key .~ (key <> "_int")
      & Proto.value .~ int2bs value
