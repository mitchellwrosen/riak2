module Riak.Internal.Error where

import Riak.Internal.IndexName (IndexName)
import Riak.Internal.Prelude

import qualified Riak.Handle.Signature as Handle

import qualified Data.ByteString as ByteString


-- TODO "Key cannot be zero-length" when putting with empty key
-- TODO "Error no bucket type"

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
       !IndexName
    -> Error 'SearchOp

  InvalidNodesError ::
       MayReturnInvalidNodes op ~ 'True
    => !Word32
    -> Error op

  -- | The schema is invalid.
  InvalidSchemaError ::
       !Text
    -> Error 'PutSchemaOp

  -- | The search failed. Typically, this means the query was malformed. Check
  -- the Solr error log, whose default location is @/var/log/riak/solr.log@.
  SearchFailedError ::
       Error 'SearchOp

  -- | A search was attempted, but either search is disabled in @riak.conf@, or
  -- the yokozuna service is not yet up.
  SearchNotEnabledError ::
       MayReturnSearchNotEnabled op ~ 'True
    => Error op

  SchemaDoesNotExistError ::
       Error 'PutIndexOp

  -- | An error was returned by the underlying handle, not Riak itself.
  HandleError ::
       !Handle.Error
    -> Error op

  -- | An error was returned by Riak, but this library couldn't parse it. Please
  -- file an issue!
  UnknownError ::
       !Text
    -> Error op

deriving stock instance Eq (Error op)
deriving stock instance Show (Error op)

-- | Operations used to index the 'Error' type.
data Op
  = DeleteOp
  | DeleteIndexOp
  | GetOp
  | GetIndexOp
  | GetSchemaOp
  | GetCrdtOp
  | PutOp
  | PutIndexOp
  | PutSchemaOp
  | SearchOp
  | UpdateCrdtOp

-- | @no_type@
type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp = 'True
  MayReturnBucketTypeDoesNotExist 'GetCrdtOp = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp = 'True
  MayReturnBucketTypeDoesNotExist 'UpdateCrdtOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False

-- | @{n_val_violation,_}@
type family MayReturnInvalidNodes (op :: Op) :: Bool where
  MayReturnInvalidNodes 'GetOp = 'True
  MayReturnInvalidNodes 'PutOp = 'True
  MayReturnInvalidNodes 'PutIndexOp = 'True
  MayReturnInvalidNodes _ = 'False

type family MayReturnSearchNotEnabled (op :: Op) :: Bool where
  MayReturnSearchNotEnabled 'DeleteIndexOp = 'True
  MayReturnSearchNotEnabled 'GetIndexOp = 'True
  MayReturnSearchNotEnabled 'GetSchemaOp = 'True
  MayReturnSearchNotEnabled 'PutIndexOp = 'True
  MayReturnSearchNotEnabled 'PutSchemaOp = 'True
  MayReturnSearchNotEnabled 'SearchOp = 'True
  MayReturnSearchNotEnabled _ = 'False

isBucketTypeDoesNotExistError :: ByteString -> Bool
isBucketTypeDoesNotExistError =
  (== "no_type")

isCrdtBucketTypeDoesNotExistError :: ByteString -> Bool
isCrdtBucketTypeDoesNotExistError =
  ByteString.isPrefixOf "Error no bucket type `<<\""

-- No index <<"foo">> found.
isIndexDoesNotExistError :: ByteString -> Bool
isIndexDoesNotExistError msg =
  ByteString.isPrefixOf "No index <<\"" msg &&
    ByteString.isSuffixOf "\">> found." msg

isInvalidNodesError :: ByteString -> Bool
isInvalidNodesError =
  ByteString.isPrefixOf "{n_val_violation"

isInvalidSchemaError :: ByteString -> Bool
isInvalidSchemaError =
  ByteString.isPrefixOf "Error storing schema"

isNotfound :: ByteString -> Bool
isNotfound =
  (== "notfound")

isMapFieldDoesNotExistError :: ByteString -> Maybe ByteString
isMapFieldDoesNotExistError bytes0 = do
  bytes1 <- ByteString.stripPrefix "{precondition,{not_present," bytes0
  -- At least give back an error with balanced { }
  ByteString.stripSuffix "}}" bytes1

isSchemaDoesNotExistError :: ByteString -> Bool
isSchemaDoesNotExistError =
  (== "Schema not found")

isSearchFailedError :: ByteString -> Bool
isSearchFailedError =
  (== "Query unsuccessful check the logs.")

isUnknownMessageCode :: ByteString -> Bool
isUnknownMessageCode =
  ByteString.isPrefixOf "Unknown message code:"
