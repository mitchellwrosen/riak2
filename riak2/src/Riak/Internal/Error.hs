module Riak.Internal.Error where

import Riak.Internal.IndexName (IndexName)
import Riak.Internal.Prelude

import qualified Riak.Handle.Signature as Handle

import qualified Data.ByteString as ByteString


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

  -- | The bucket type was "invalid" for some reason (operation-specific).
  InvalidBucketTypeError ::
      !ByteString
    -> Error 'SetBucketTypeIndexOp

  -- | The search index does not exist.
  IndexDoesNotExistError ::
       MayReturnIndexDoesNotExist op ~ 'True
    => !IndexName
    -> Error op

  InvalidNodesError ::
       MayReturnInvalidNodes op ~ 'True
    => Error op

  -- | The schema is invalid.
  InvalidSchemaError ::
       !Text
    -> Error 'PutSchemaOp

  -- | Riak is overloaded.
  OverloadError ::
       MayReturnOverload op ~ 'True
    => Error op

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
       !Handle.HandleError
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
  | GetBucketOp
  | GetBucketTypeOp
  | GetIndexOp
  | GetSchemaOp
  | GetCrdtOp
  | ListBucketsOp
  | ListKeysOp
  | PutOp
  | PutIndexOp
  | PutSchemaOp
  | SearchOp
  | SetBucketTypeIndexOp
  | UpdateCrdtOp

type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp = 'True
  MayReturnBucketTypeDoesNotExist 'GetCrdtOp = 'True
  MayReturnBucketTypeDoesNotExist 'ListBucketsOp = 'True
  MayReturnBucketTypeDoesNotExist 'ListKeysOp = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp = 'True
  MayReturnBucketTypeDoesNotExist 'UpdateCrdtOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False

type family MayReturnIndexDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'SearchOp = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp = 'True
  MayReturnBucketTypeDoesNotExist _ = 'False

-- | @{n_val_violation,_}@
type family MayReturnInvalidNodes (op :: Op) :: Bool where
  MayReturnInvalidNodes 'GetOp = 'True
  MayReturnInvalidNodes 'PutOp = 'True
  MayReturnInvalidNodes 'PutIndexOp = 'True
  MayReturnInvalidNodes 'SetBucketTypeIndexOp = 'True
  MayReturnInvalidNodes _ = 'False

type family MayReturnOverload (op :: Op) :: Bool where
  MayReturnOverload 'DeleteOp = 'True
  MayReturnOverload 'GetOp = 'True
  MayReturnOverload 'PutOp = 'True
  MayReturnOverload _ = 'False

type family MayReturnSearchNotEnabled (op :: Op) :: Bool where
  MayReturnSearchNotEnabled 'DeleteIndexOp = 'True
  MayReturnSearchNotEnabled 'GetIndexOp = 'True
  MayReturnSearchNotEnabled 'GetSchemaOp = 'True
  MayReturnSearchNotEnabled 'PutIndexOp = 'True
  MayReturnSearchNotEnabled 'PutSchemaOp = 'True
  MayReturnSearchNotEnabled 'SearchOp = 'True
  MayReturnSearchNotEnabled _ = 'False

-- no_type
isBucketTypeDoesNotExistError0 :: ByteString -> Bool
isBucketTypeDoesNotExistError0 =
  (== "no_type")

-- Error no bucket type `<<"aa">>`
isBucketTypeDoesNotExistError1 :: ByteString -> Bool
isBucketTypeDoesNotExistError1 =
  ByteString.isPrefixOf "Error no bucket type `<<\""

-- Invalid bucket properties: not_active
isBucketTypeDoesNotExistError2 :: ByteString -> Bool
isBucketTypeDoesNotExistError2 =
  (== "Invalid bucket properties: not_active")

-- Invalid bucket type: <<"foo">>
isBucketTypeDoesNotExistError3 :: ByteString -> Bool
isBucketTypeDoesNotExistError3 =
  ByteString.isPrefixOf "Invalid bucket type: <<\""

-- No bucket-type named 'foo'
isBucketTypeDoesNotExistError4 :: ByteString -> Bool
isBucketTypeDoesNotExistError4 =
  ByteString.isPrefixOf "No bucket-type named '"

-- Invalid bucket properties: [{search_index,<<"foo does not exist">>}]
isIndexDoesNotExistError0 :: ByteString -> Bool
isIndexDoesNotExistError0 msg =
  ByteString.isPrefixOf "Invalid bucket properties: [{search_index,<<\"" msg &&
    ByteString.isSuffixOf " does not exist\">>}]" msg

-- No index <<"foo">> found.
isIndexDoesNotExistError1 :: ByteString -> Bool
isIndexDoesNotExistError1 msg =
  ByteString.isPrefixOf "No index <<\"" msg &&
    ByteString.isSuffixOf "\">> found." msg

isInvalidNodesError0 :: ByteString -> Bool
isInvalidNodesError0 =
  ByteString.isPrefixOf "{n_val_violation"

isInvalidNodesError1 :: ByteString -> Bool
isInvalidNodesError1 =
  ByteString.isPrefixOf "Invalid bucket properties: [{n_val,"

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

isOverloadError :: ByteString -> Bool
isOverloadError =
  (== "overload")

isSchemaDoesNotExistError :: ByteString -> Bool
isSchemaDoesNotExistError =
  (== "Schema not found")

isSearchFailedError :: ByteString -> Bool
isSearchFailedError =
  (== "Query unsuccessful check the logs.")

isUnknownMessageCode :: ByteString -> Bool
isUnknownMessageCode =
  ByteString.isPrefixOf "Unknown message code:"

-- Parse a response from a handle operation, for the common case that Riak
-- errors map to errors, and Riak successes map to successes.
fromHandleResult ::
     (ByteString -> Either (Error op) resp')
  -> (resp -> resp')
  -> IO (Either Handle.HandleError (Either ByteString resp))
  -> IO (Either (Error op) resp')
fromHandleResult fromErr fromOk =
  fmap $ \case
    Left err ->
      Left (HandleError err)

    Right (Left err) ->
      fromErr err

    Right (Right response) ->
      Right (fromOk response)
