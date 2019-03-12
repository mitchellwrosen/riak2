module Riak.BucketType
  ( -- * Properties
    getBucketType
  , getCounterBucketType
  , getHyperLogLogBucketType
  , getMapBucketType
  , getSetBucketType
  , setBucketTypeIndex
  , unsetBucketTypeIndex
    -- * Full traversals
  , listBuckets
  , streamBuckets
    -- * Bucket type
  , BucketType
  , defaultBucketType
  ) where

import RiakBucketType
