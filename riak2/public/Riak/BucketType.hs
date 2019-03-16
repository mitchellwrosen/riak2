module Riak.BucketType
  ( -- * Bucket type
    BucketType
  , defaultBucketType
    -- * Properties
  , getBucketType
  , getCounterBucketType
  , getHyperLogLogBucketType
  , getMapBucketType
  , getSetBucketType
    -- ** Search index
  , setBucketTypeIndex
  , unsetBucketTypeIndex
    -- * Full key traversals
  , listBuckets
  , streamBuckets
  ) where

import RiakBucketType
