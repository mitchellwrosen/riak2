module Riak.BucketProps
  ( SomeBucketProps(..)
  , BucketProps(..)
  , CounterBucketProps(..)
  , HyperLogLogBucketProps(..)
  , MapBucketProps(..)
  , SetBucketProps(..)
  , ConflictResolution(..)
  , NotfoundBehavior(..)
  , PruneContextSettings(..)
  ) where

import RiakBucketProps            (BucketProps(..), ConflictResolution(..),
                                   PruneContextSettings(..))
import RiakCounterBucketProps     (CounterBucketProps(..))
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps(..))
import RiakMapBucketProps         (MapBucketProps(..))
import RiakNotfoundBehavior       (NotfoundBehavior(..))
import RiakSetBucketProps         (SetBucketProps(..))
import RiakSomeBucketProps        (SomeBucketProps(..))
