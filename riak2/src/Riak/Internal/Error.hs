module Riak.Internal.Error where

import Riak.Internal.Prelude

import qualified Riak.Handle.Signature as Handle

import ByteString

-- TODO "Key cannot be zero-length" when putting with empty key

-- | Error responses that Riak may return, plus a generic "handle error" that
-- occurs when something goes wrong with the underlying connection.
--
-- The goal here is to parse Riak's rather terse error strings into an ADT that
-- can be meaningfully pattern matched on, and incorporated into business logic.
--
-- However, Riak can return /many/ different kinds of errors, so those that are
-- not yet included here will be returned as a generic 'UnknownError'.
--
-- If you encounter an 'UnknownError', please open an issue about it!
data Error :: Op -> Type where
  BucketTypeDoesNotExistError ::
       MayReturnBucketTypeDoesNotExist op ~ 'True
    => !ByteString
    -> Error op

  -- | The search index does not exist.
  IndexDoesNotExistError ::
       !Text
    -> Error 'SearchOp

  InvalidReplicasError ::
       MayReturnInvalidReplicas op ~ 'True
    => !Word32
    -> Error op

  -- | The search failed. Typically, this means the query was malformed. Check
  -- the Solr error log, whose default location is @/var/log/riak/solr.log@.
  SearchFailedError ::
       Error 'SearchOp

  -- | A search was attempted, but either search is disabled in @riak.conf@, or
  -- the yokozuna service is not yet up.
  SearchNotEnabledError ::
       Error 'SearchOp

  HandleError ::
       !Handle.Error
    -> Error op

  UnknownError ::
       !Text
    -> Error op

deriving stock instance Eq (Error op)
deriving stock instance Show (Error op)

-- | Operations used to index the 'Error' type.
data Op
  = DeleteOp
  | GetOp
  | PutOp
  | SearchOp

-- | @no_type@
type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False

-- | @{n_val_violation,_}@
type family MayReturnInvalidReplicas (op :: Op) :: Bool where
  MayReturnInvalidReplicas 'GetOp = 'True
  MayReturnInvalidReplicas 'PutOp = 'True
  MayReturnInvalidReplicas _ = 'False

isBucketTypeDoesNotExistError :: ByteString -> Bool
isBucketTypeDoesNotExistError =
  (== "no_type")

-- No index <<"foo">> found.
isIndexDoesNotExistError :: ByteString -> Bool
isIndexDoesNotExistError msg =
  ByteString.isPrefixOf "No index <<\"" msg &&
    ByteString.isSuffixOf "\">> found." msg

isInvalidReplicasError :: ByteString -> Bool
isInvalidReplicasError =
  ByteString.isPrefixOf "{n_val_violation"

isSearchFailedError :: ByteString -> Bool
isSearchFailedError =
  (== "Query unsuccessful check the logs.")

isSearchNotEnabledError :: ByteString -> Bool
isSearchNotEnabledError =
  (== "Unknown message code: 27")
