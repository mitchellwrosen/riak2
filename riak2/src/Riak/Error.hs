module Riak.Error
  ( Error(..)
  , Op(..)
  ) where

import Riak.Internal.Prelude

-- | Error responses that Riak may return.
--
-- The goal here is to parse Riak's rather terse error strings into an ADT that
-- can be meaningfully pattern matched on, and incorporated into business logic.
--
-- However, Riak can return /many/ different kinds of errors, so those that are
-- not yet included here will be returned as a generic 'UnknownError'.
--
-- If you encounter an 'UnknownError', please open an issue about it!
data Error :: Op -> Type where
  BucketTypeDoesNotExist ::
       MayReturnBucketTypeDoesNotExist op ~ 'True
    => !ByteString
    -> Error op

  UnknownError ::
       !Text
    -> Error op

deriving stock instance Show (Error op)

-- | Operations used to index the 'Error' type.
data Op
  = PutOp

-- | @no_type@
type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'PutOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False
