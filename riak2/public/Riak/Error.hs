module Riak.Error
  ( -- * Error type
    Error(..)
  , Op(..)
    -- * Error type aliases
  , DeleteIndexError
  , GetBucketError
  , GetBucketTypeError
  , GetCounterError
  , GetError
  , GetHyperLogLogError
  , GetIndexError
  , GetMapError
  , GetSchemaError
  , GetSetError
  , IncrementCounterError
  , ListBucketsError
  , ListKeysError
  , MapReduceBucketError
  , PutError
  , PutIndexError
  , PutMapError
  , PutSchemaError
  , PutSetError
  , QueryIndexError
  , SearchError
  , SetBucketTypeIndexError
  , UpdateHyperLogLogError
    -- * Internal type families
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  ) where

import RiakError
