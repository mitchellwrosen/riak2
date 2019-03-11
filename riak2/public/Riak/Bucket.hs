module Riak.Bucket
  ( -- * Properties
    getBucket
  , getCounterBucket
  , getHyperLogLogBucket
  , getMapBucket
  , getSetBucket
  , setBucketIndex
  , unsetBucketIndex
  , resetBucket
    -- * Search
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
    -- * Full traversals
  , listKeys
  , streamKeys
    -- * Bucket
  , Bucket(..)
  , bucketBucketType
  , bucketBucketSegment
  ) where

import RiakBucket
