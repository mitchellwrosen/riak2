module RiakSomeBucketProps where

import RiakBucketProps            (BucketProps)
import RiakCounterBucketProps     (CounterBucketProps)
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps)
import RiakMapBucketProps         (MapBucketProps)
import RiakSetBucketProps         (SetBucketProps)

import qualified RiakBucketProps            as BucketProps
import qualified RiakCounterBucketProps     as CounterBucketProps
import qualified RiakHyperLogLogBucketProps as HyperLogLogBucketProps
import qualified RiakMapBucketProps         as MapBucketProps
import qualified RiakSetBucketProps         as SetBucketProps

import Control.Lens ((^.))

import qualified Data.Riak.Proto as Proto


-- | Bucket properties of some bucket type whose @datatype@ is unknown ahead of
-- time.
data SomeBucketProps
  = SomeBucketProps BucketProps
  | SomeCounterBucketProps CounterBucketProps
  | SomeHyperLogLogBucketProps HyperLogLogBucketProps
  | SomeMapBucketProps MapBucketProps
  | SomeSetBucketProps SetBucketProps
  deriving stock (Eq, Show)

fromProto :: Proto.RpbBucketProps -> SomeBucketProps
fromProto props =
  case props ^. Proto.maybe'datatype of
    Just "counter" ->
      SomeCounterBucketProps (CounterBucketProps.fromProto props)
    Just "hll" ->
      SomeHyperLogLogBucketProps (HyperLogLogBucketProps.fromProto props)
    Just "map" ->
      SomeMapBucketProps (MapBucketProps.fromProto props)
    Just "set" ->
      SomeSetBucketProps (SetBucketProps.fromProto props)
    _ ->
      SomeBucketProps (BucketProps.fromProto props)
