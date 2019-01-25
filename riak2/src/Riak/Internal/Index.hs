module Riak.Internal.Index where

import Riak.Internal.IndexValue (IndexValue)
import Riak.Internal.Panic
import Riak.Internal.Prelude
import Riak.Internal.Utils
import Riak.Proto               (Pair)

import qualified Riak.Internal.IndexValue as IndexValue
import qualified Riak.Internal.Proto.Pair as Pair

import qualified Data.ByteString as ByteString


-- TODO Index values should be a set
-- | A secondary index.
data Index
  = forall a.
    Index !ByteString !(IndexValue a)

deriving stock instance Show Index

-- | Binary index smart constructor.
binary :: ByteString -> ByteString -> Index
binary index value =
  Index index (IndexValue.Binary value)

-- | Integer index smart constructor.
integer :: ByteString -> Int64 -> Index
integer index value =
  Index index (IndexValue.Integer value)

fromPair :: Pair -> Index
fromPair =
  Pair.toTuple >>> \case
    (ByteString.stripSuffix "_bin" -> Just k, Just v) ->
      Index k (IndexValue.Binary v)

    (ByteString.stripSuffix "_int" -> Just k, Just v) ->
      Index k (IndexValue.Integer (bs2int v))

    (k, v) ->
      impurePanic "Riak.Internal.Index.fromPair"
        ( ("key",   k)
        , ("value", v)
        )

toPair :: Index -> Pair
toPair = \case
  Index k (IndexValue.Binary v) ->
    Pair.fromTuple (k <> "_bin", Just v)

  Index k (IndexValue.Integer v) ->
    Pair.fromTuple (k <> "_int", Just (int2bs v))
