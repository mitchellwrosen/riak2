module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
  , bucketBucketType
  , bucketBucketSegment
    -- * Properties
  , getBucket
  , getCounterBucket
  , getHyperLogLogBucket
  , getMapBucket
  , getSetBucket
  , resetBucket
    -- ** Search index
  , setBucketIndex
  , unsetBucketIndex
    -- * Secondary index search
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
    -- * Full key traversals
  , listKeys
  , streamKeys
  , ListKeysOpts(..)
    -- ** *With variants
  , listKeysWith
  , streamKeysWith
  ) where

import RiakBucket
