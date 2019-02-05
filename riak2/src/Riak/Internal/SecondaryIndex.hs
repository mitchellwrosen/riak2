module Riak.Internal.SecondaryIndex where

import Riak.Internal.Panic
import Riak.Internal.Prelude
import Riak.Internal.SecondaryIndexValue (SecondaryIndexValue)
import Riak.Internal.Utils
import Riak.Proto                        (Pair)

import qualified Riak.Internal.Proto.Pair          as Pair
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue

import qualified Data.ByteString as ByteString


-- TODO Index values should be a set
-- | A secondary index.
data SecondaryIndex
  = forall a.
    SecondaryIndex !ByteString !(SecondaryIndexValue a)

deriving stock instance Show SecondaryIndex

-- | Binary index smart constructor.
binary ::
     ByteString -- ^ Index name
  -> ByteString -- ^ Value
  -> SecondaryIndex
binary index value =
  SecondaryIndex index (SecondaryIndexValue.Binary value)

-- | Integer index smart constructor.
integer ::
     ByteString -- ^ Index name
  -> Int64 -- ^ Value
  -> SecondaryIndex
integer index value =
  SecondaryIndex index (SecondaryIndexValue.Integer value)

fromPair :: Pair -> SecondaryIndex
fromPair =
  Pair.toTuple >>> \case
    (ByteString.stripSuffix "_bin" -> Just k, Just v) ->
      SecondaryIndex k (SecondaryIndexValue.Binary v)

    (ByteString.stripSuffix "_int" -> Just k, Just v) ->
      SecondaryIndex k (SecondaryIndexValue.Integer (bs2int v))

    (k, v) ->
      impurePanic "Riak.Internal.SecondaryIndex.fromPair"
        ( ("key",   k)
        , ("value", v)
        )

toPair :: SecondaryIndex -> Pair
toPair = \case
  SecondaryIndex k (SecondaryIndexValue.Binary v) ->
    Pair.fromTuple (k <> "_bin", Just v)

  SecondaryIndex k (SecondaryIndexValue.Integer v) ->
    Pair.fromTuple (k <> "_int", Just (int2bs v))
