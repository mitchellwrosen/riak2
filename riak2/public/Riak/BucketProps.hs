module Riak.BucketProps
  ( SomeBucketProps(..)
  , BucketProps(..)
  , CounterBucketProps(..)
  , HyperLogLogBucketProps(..)
  , MapBucketProps(..)
  , SetBucketProps(..)
  , ConflictResolution(..)
  , NotfoundBehavior(..)
  ) where

import RiakBucketProps            (BucketProps(..), ConflictResolution(..))
import RiakCounterBucketProps     (CounterBucketProps(..))
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps(..))
import RiakMapBucketProps         (MapBucketProps(..))
import RiakNotfoundBehavior       (NotfoundBehavior(..))
import RiakSetBucketProps         (SetBucketProps(..))
import RiakSomeBucketProps        (SomeBucketProps(..))
