module Riak.Error
  ( -- * Error type
    Error(..)
  , Op(..)
    -- * Error type aliases
  , DeleteError
  , DeleteIndexError
  , GetBucketError
  , GetBucketTypeError
  , GetConvergentCounterError
  , GetConvergentHyperLogLogError
  , GetConvergentMapError
  , GetConvergentSetError
  , GetIndexError
  , GetError
  , GetSchemaError
  , ListBucketsError
  , ListKeysError
  , PutConvergentMapError
  , PutConvergentSetError
  , PutError
  , PutIndexError
  , PutSchemaError
  , QueryExactError
  , QueryRangeError
  , SearchError
  , SetBucketTypeIndexError
  , UpdateConvergentCounterError
  , UpdateConvergentHyperLogLogError
    -- * Internal type families
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  , MayReturnOverload
  , MayReturnSearchNotEnabled
  ) where

import RiakError
