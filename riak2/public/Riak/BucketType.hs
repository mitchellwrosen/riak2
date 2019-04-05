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
  , ListBucketsOpts(..)
    -- ** *With variants
  , listBucketsWith
  , streamBucketsWith
  ) where

import RiakBucketType
