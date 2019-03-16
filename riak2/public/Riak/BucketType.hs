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
    -- ** *With variants
  , listBucketsWith
  , streamBucketsWith
  ) where

import RiakBucketType
