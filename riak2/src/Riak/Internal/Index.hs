module Riak.Internal.Index where

import Riak.Internal.Panic
import Riak.Internal.Prelude
import Riak.Internal.Utils
import Riak.Proto (Pair)

import qualified Riak.Internal.Proto.Pair as Pair

import qualified Data.ByteString as ByteString


-- TODO Index values should be a set
-- | A secondary index.
data Index
  = IndexInt !ByteString !Int64
  | IndexBin !ByteString !ByteString
  deriving (Eq, Show)

fromPair :: Pair -> Index
fromPair =
  Pair.toTuple >>> \case
    (ByteString.stripSuffix "_bin" -> Just k, Just v) ->
      IndexBin k v

    (ByteString.stripSuffix "_int" -> Just k, Just v) ->
      IndexInt k (bs2int v)

    (k, v) ->
      impurePanic "Riak.Internal.Content.parseIndex"
        ( ("key",   k)
        , ("value", v)
        )

toPair :: Index -> Pair
toPair = \case
  IndexInt k v ->
    Pair.fromTuple (k <> "_int", Just (int2bs v))

  IndexBin k v ->
    Pair.fromTuple (k <> "_bin", Just v)
