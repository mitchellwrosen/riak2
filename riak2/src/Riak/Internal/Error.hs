module Riak.Internal.Error where

import Riak.Internal.Prelude

import qualified Data.Text as Text

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

  InvalidN ::
       MayReturnInvalidN op ~ 'True
    => !Word32
    -> Error op

  UnknownError ::
       !Text
    -> Error op

deriving stock instance Show (Error op)

-- | Operations used to index the 'Error' type.
data Op
  = GetOp
  | PutOp

-- | @no_type@
type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False

-- | @{n_val_violation,_}
type family MayReturnInvalidN (op :: Op) :: Bool where
  MayReturnInvalidN 'GetOp = 'True
  MayReturnInvalidN 'PutOp = 'True
  MayReturnInvalidN _ = 'False

isBucketTypeDoesNotExist :: Text -> Bool
isBucketTypeDoesNotExist =
  (== "no_type")

isInvalidN :: Text -> Bool
isInvalidN =
  Text.isPrefixOf "{n_val_violation"
