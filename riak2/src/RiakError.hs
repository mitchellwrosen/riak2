module RiakError where

import RiakBucketInternal     (Bucket(..))
import RiakBucketTypeInternal (defaultBucketType)
import RiakIndexName          (IndexName)

import qualified RiakHandle as Handle (HandleError)

import qualified Data.Attoparsec.ByteString       as Atto (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as ByteString


-- TODO "Key cannot be zero-length" when putting with empty key
--
-- TODO retry on insufficient vnodes
--        - list keys
--
-- TODO get "{insufficient_vnodes,0,need,2}"
-- TODO get "{pr_val_unsatisfied,3,2}"
-- TODO put/delete "{pw_val_unsatisfied,3,2}"

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
    => ByteString
    -> Error op

  -- | Secondary indexes are not supported by this bucket's backend.
  SecondaryIndexesNotSupportedError ::
       Bucket
    -> Error 'SecondaryIndexQueryOp

  -- | The search index does not exist.
  IndexDoesNotExistError ::
       MayReturnIndexDoesNotExist op ~ 'True
    => IndexName
    -> Error op

  -- | The index is associated with buckets.
  IndexHasAssociatedBucketsError ::
       MayReturnIndexHasAssociatedBuckets op ~ 'True
    => IndexName
    -> [Bucket]
    -> Error op

  -- | Insufficient nodes are available to service the request.
  InsufficientNodesError ::
       MayReturnInsufficientNodes op ~ 'True
    => Error op

  -- | The bucket type was "invalid" for some reason (operation-specific).
  InvalidBucketTypeError ::
       MayReturnInvalidBucketType op ~ 'True
    => ByteString
    -> Error op

  InvalidNodesError ::
       MayReturnInvalidNodes op ~ 'True
    => Error op

  -- | The schema is invalid.
  InvalidSchemaError ::
       Text
    -> Error 'PutSchemaOp

  -- | Riak is overloaded.
  OverloadError ::
       MayReturnOverload op ~ 'True
    => Error op

  -- | A search-related operation was attempted, but either search is disabled
  -- in @riak.conf@, or the search service is not yet up.
  SearchNotEnabledError ::
       MayReturnSearchNotEnabled op ~ 'True
    => Error op

  -- | The search failed. Typically, this means the query was malformed. Check
  -- the Solr error log, whose default location is @/var/log/riak/solr.log@.
  SearchFailedError ::
       Error 'SearchOp

  SchemaDoesNotExistError ::
       Error 'PutIndexOp

  -- | An error was returned by the underlying handle, not Riak itself.
  HandleError ::
       Handle.HandleError
    -> Error op

  -- | An error was returned by Riak, but this library couldn't parse it. Please
  -- file an issue!
  UnknownError ::
       Text
    -> Error op

deriving stock instance Eq (Error op)
deriving stock instance Show (Error op)

-- | Operations used to index the 'Error' type.
data Op
  = DeleteIndexOp
  | GetBucketOp
  | GetCrdtOp
  | GetIndexOp
  | GetOp
  | GetSchemaOp
  | GetSomeBucketOp
  | ListBucketsOp
  | ListKeysOp
  | MapReduceBucketOp
  | MapReduceSecondaryIndexOp
  | PutIndexOp
  | PutOp
  | PutSchemaOp
  | SearchOp
  | SecondaryIndexQueryOp
  | SetBucketIndexOp
  | SetBucketTypeIndexOp
  | UnsetBucketIndexOp
  | UpdateCrdtOp

type DeleteIndexError              = Error 'DeleteIndexOp
type GetBucketError                = Error 'GetSomeBucketOp
type GetBucketTypeError            = Error 'GetSomeBucketOp
type GetCounterBucketError         = Error 'GetBucketOp
type GetCounterBucketTypeError     = Error 'GetBucketOp
type GetCounterError               = Error 'GetCrdtOp
type GetError                      = Error 'GetOp
type GetHyperLogLogBucketError     = Error 'GetBucketOp
type GetHyperLogLogBucketTypeError = Error 'GetBucketOp
type GetHyperLogLogError           = Error 'GetCrdtOp
type GetIndexError                 = Error 'GetIndexOp
type GetMapBucketError             = Error 'GetBucketOp
type GetMapBucketTypeError         = Error 'GetBucketOp
type GetMapError                   = Error 'GetCrdtOp
type GetSchemaError                = Error 'GetSchemaOp
type GetSetBucketError             = Error 'GetBucketOp
type GetSetBucketTypeError         = Error 'GetBucketOp
type GetSetError                   = Error 'GetCrdtOp
type ListBucketsError              = Error 'ListBucketsOp
type ListKeysError                 = Error 'ListKeysOp
type MapReduceBinaryIndexError     = Error 'MapReduceSecondaryIndexOp
type MapReduceBucketError          = Error 'MapReduceBucketOp
type MapReduceIntIndexError        = Error 'MapReduceSecondaryIndexOp
type PutError                      = Error 'PutOp
type PutIndexError                 = Error 'PutIndexOp
type PutMapError                   = Error 'UpdateCrdtOp
type PutSchemaError                = Error 'PutSchemaOp
type PutSetError                   = Error 'UpdateCrdtOp
type QueryExactError               = Error 'SecondaryIndexQueryOp
type QueryRangeError               = Error 'SecondaryIndexQueryOp
type SearchError                   = Error 'SearchOp
type SetBucketIndexError           = Error 'SetBucketIndexOp
type SetBucketTypeIndexError       = Error 'SetBucketTypeIndexOp
type UnsetBucketIndexError         = Error 'UnsetBucketIndexOp
type UpdateCounterError            = Error 'UpdateCrdtOp
type UpdateHyperLogLogError        = Error 'UpdateCrdtOp

type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp                = 'True
  MayReturnBucketTypeDoesNotExist 'GetBucketOp          = 'True
  MayReturnBucketTypeDoesNotExist 'GetCrdtOp            = 'True
  MayReturnBucketTypeDoesNotExist 'GetSomeBucketOp      = 'True
  MayReturnBucketTypeDoesNotExist 'ListBucketsOp        = 'True
  MayReturnBucketTypeDoesNotExist 'ListKeysOp           = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp                = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketIndexOp     = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp = 'True
  MayReturnBucketTypeDoesNotExist 'UnsetBucketIndexOp   = 'True
  MayReturnBucketTypeDoesNotExist 'UpdateCrdtOp         = 'True
  MayReturnBucketTypeDoesNotExist _                     = 'False

type family MayReturnIndexDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'SearchOp             = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketIndexOp     = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp = 'True
  MayReturnBucketTypeDoesNotExist _                     = 'False

type family MayReturnIndexHasAssociatedBuckets  (op :: Op) :: Bool where
  MayReturnIndexHasAssociatedBuckets 'DeleteIndexOp = 'True
  MayReturnIndexHasAssociatedBuckets 'PutIndexOp    = 'True
  MayReturnIndexHasAssociatedBuckets _              = 'False

type family MayReturnInsufficientNodes (op :: Op) :: Bool where
  MayReturnInsufficientNodes 'SecondaryIndexQueryOp = 'True
  MayReturnInsufficientNodes _                      = 'False

type family MayReturnInvalidBucketType (op :: Op) :: Bool where
  MayReturnInvalidBucketType 'GetBucketOp          = 'True
  MayReturnInvalidBucketType 'SetBucketTypeIndexOp = 'True
  MayReturnInvalidBucketType _                     = 'False

-- | @{n_val_violation,_}@
type family MayReturnInvalidNodes (op :: Op) :: Bool where
  MayReturnInvalidNodes 'GetOp                = 'True
  MayReturnInvalidNodes 'PutOp                = 'True
  MayReturnInvalidNodes 'PutIndexOp           = 'True
  MayReturnInvalidNodes 'SetBucketIndexOp     = 'True
  MayReturnInvalidNodes 'SetBucketTypeIndexOp = 'True
  MayReturnInvalidNodes _                     = 'False

-- | @overload@
type family MayReturnOverload (op :: Op) :: Bool where
  MayReturnOverload 'GetOp    = 'True
  MayReturnOverload 'PutOp    = 'True
  MayReturnOverload _         = 'False

type family MayReturnSearchNotEnabled (op :: Op) :: Bool where
  MayReturnSearchNotEnabled 'DeleteIndexOp = 'True
  MayReturnSearchNotEnabled 'GetIndexOp    = 'True
  MayReturnSearchNotEnabled 'GetSchemaOp   = 'True
  MayReturnSearchNotEnabled 'PutIndexOp    = 'True
  MayReturnSearchNotEnabled 'PutSchemaOp   = 'True
  MayReturnSearchNotEnabled 'SearchOp      = 'True
  MayReturnSearchNotEnabled _              = 'False

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

-- Can't delete index with associate buckets [{<<\"objects\">>,<<\"foo\">>},\n    {<<\"objects\">>,<<\"bar\">>}]
isHasAssociatedBucketsError :: ByteString -> Maybe [Bucket]
isHasAssociatedBucketsError err = do
  bytes :: ByteString <-
    ByteString.stripPrefix "Can't delete index with associate buckets" err

  case Atto.parseOnly bucketsParser bytes of
    Left _ ->
      Nothing

    Right buckets ->
      Just buckets

  where
    bucketsParser :: Atto.Parser [Bucket]
    bucketsParser = do
      Atto.skipSpace
      Atto.char '[' *> Atto.skipSpace

      buckets :: [Bucket] <-
        Atto.sepBy1'
          bucketOrBucketTypeParser
          (Atto.char ',' *> Atto.skipSpace)

      Atto.char ']' *> Atto.skipSpace
      Atto.endOfInput
      pure buckets

    bucketOrBucketTypeParser :: Atto.Parser Bucket
    bucketOrBucketTypeParser =
      bucketTypeParser <|> bucketParser

    bucketTypeParser :: Atto.Parser Bucket
    bucketTypeParser = do
      Atto.char '{' *> Atto.skipSpace
      bucketType <- binaryParser
      Atto.char ',' *> Atto.skipSpace
      bucket <- binaryParser
      Atto.char '}' $> Bucket bucketType bucket

    bucketParser :: Atto.Parser Bucket
    bucketParser =
      Bucket defaultBucketType <$>
        binaryParser

    binaryParser :: Atto.Parser ByteString
    binaryParser = do
      _      <- Atto.char '<'
      _      <- Atto.char '<'
      _      <- Atto.char '"'
      bucket <- Atto.takeWhile (/= '"')
      _      <- Atto.char '"'
      _      <- Atto.char '>'
      _      <- Atto.char '>'
      Atto.skipSpace $> bucket

-- Invalid bucket properties: [{search_index,<<"foo does not exist">>}]
isIndexDoesNotExistError0 :: ByteString -> Bool
isIndexDoesNotExistError0 msg =
  ByteString.isPrefixOf "Invalid bucket properties: [{search_index," msg &&
    ByteString.isSuffixOf " does not exist\">>}]" msg

-- No index <<"foo">> found.
isIndexDoesNotExistError1 :: ByteString -> Bool
isIndexDoesNotExistError1 msg =
  ByteString.isPrefixOf "No index <<\"" msg &&
    ByteString.isSuffixOf "\">> found." msg

isInsufficientNodesError :: ByteString -> Bool
isInsufficientNodesError =
  (== "{error,insufficient_vnodes_available}")

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

isOverloadError :: ByteString -> Bool
isOverloadError =
  (== "overload")

isSchemaDoesNotExistError :: ByteString -> Bool
isSchemaDoesNotExistError =
  (== "Schema not found")

isSearchFailedError :: ByteString -> Bool
isSearchFailedError =
  (== "Query unsuccessful check the logs.")

isSecondaryIndexesNotSupportedError :: ByteString -> Bool
isSecondaryIndexesNotSupportedError =
  ByteString.isPrefixOf "{error,{indexes_not_supported,"

isUnknownMessageCode :: ByteString -> Bool
isUnknownMessageCode =
  ByteString.isPrefixOf "Unknown message code:"
