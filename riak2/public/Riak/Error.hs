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
  , UpdateCounterError
  , UpdateHyperLogLogError
    -- * Internal type families
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  ) where

import RiakError
