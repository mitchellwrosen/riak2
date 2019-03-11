module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , getBucket
  , getCounterBucket
  , getHyperLogLogBucket
  , getMapBucket
  , getSetBucket
  , setBucketIndex
  , unsetBucketIndex
  , resetBucket
    -- ** Search
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
    -- ** Full traversals
  , listKeys
  , streamKeys
  ) where

import RiakBucket
