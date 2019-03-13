module Riak.BucketProps
  ( SomeBucketProps(..)
  , BucketProps(..)
  , CounterBucketProps(..)
  , HyperLogLogBucketProps(..)
  , MapBucketProps(..)
  , SetBucketProps(..)
  , ConflictResolution(..)
  , PruneContextSettings(..)
  ) where

import RiakBucketProps            (BucketProps(..), ConflictResolution(..),
                                   PruneContextSettings(..))
import RiakCounterBucketProps     (CounterBucketProps(..))
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps(..))
import RiakMapBucketProps         (MapBucketProps(..))
import RiakSetBucketProps         (SetBucketProps(..))
import RiakSomeBucketProps        (SomeBucketProps(..))
