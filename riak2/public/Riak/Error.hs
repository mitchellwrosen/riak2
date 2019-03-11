module Riak.Error
  ( -- * Error type
    Error(..)
  , Op(..)
    -- * Error type aliases
  , DeleteError
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
  , QueryExactError
  , QueryRangeError
  , SearchError
  , SetBucketTypeIndexError
  , UpdateCounterError
  , UpdateHyperLogLogError
    -- * Internal type families
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  , MayReturnOverload
  , MayReturnSearchNotEnabled
  ) where

import RiakError
