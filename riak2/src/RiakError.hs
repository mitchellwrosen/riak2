-- TODO more exhausive zero-length bucket/key coverage

module RiakError where

import RiakBucketInternal     (Bucket(..))
import RiakBucketTypeInternal (BucketType, defaultBucketType)
import RiakIndexName          (IndexName)
import RiakKeyInternal        (Key)

import qualified RiakHandleError as HandleError (HandleError)

import qualified Data.Attoparsec.ByteString       as Atto (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as ByteString


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

  InvalidBucketError ::
       MayReturnInvalidBucket op ~ 'True
    => Bucket
    -> Error op

  -- | The bucket type was "invalid" for some reason (operation-specific).
  InvalidBucketTypeError ::
       MayReturnInvalidBucketType op ~ 'True
    => BucketType
    -> Error op

  InvalidKeyError ::
       MayReturnInvalidKey op ~ 'True
    => Key
    -> Error op

  InvalidNodesError ::
       MayReturnInvalidNodes op ~ 'True
    => Error op

  -- | The schema is invalid.
  InvalidSchemaError ::
       Text
    -> Error 'PutSchemaOp

  -- | Riak is overloaded.
  --
  -- TODO retry on overload
  OverloadError ::
       MayReturnOverload op ~ 'True
    => Error op

  -- | The search failed. Typically, this means the query was malformed. Check
  -- the Solr error log, whose default location is @/var/log/riak/solr.log@.
  SearchFailedError ::
       Error 'SearchOp

  SchemaDoesNotExistError ::
       Error 'PutIndexOp

  -- | An error was returned by the underlying handle.
  HandleError ::
       HandleError.HandleError
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
  | ResetBucketOp
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
type QueryIndexError               = Error 'SecondaryIndexQueryOp
type ResetBucketError              = Error 'ResetBucketOp
type SearchError                   = Error 'SearchOp
type SetBucketIndexError           = Error 'SetBucketIndexOp
type SetBucketTypeIndexError       = Error 'SetBucketTypeIndexOp
type UnsetBucketIndexError         = Error 'UnsetBucketIndexOp
type UpdateCounterError            = Error 'UpdateCrdtOp
type UpdateHyperLogLogError        = Error 'UpdateCrdtOp

type family MayReturnBucketTypeDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'GetOp                 = 'True
  MayReturnBucketTypeDoesNotExist 'GetBucketOp           = 'True
  MayReturnBucketTypeDoesNotExist 'GetCrdtOp             = 'True
  MayReturnBucketTypeDoesNotExist 'GetSomeBucketOp       = 'True
  MayReturnBucketTypeDoesNotExist 'ListBucketsOp         = 'True
  MayReturnBucketTypeDoesNotExist 'ListKeysOp            = 'True
  MayReturnBucketTypeDoesNotExist 'PutOp                 = 'True
  MayReturnBucketTypeDoesNotExist 'SecondaryIndexQueryOp = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketIndexOp      = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp  = 'True
  MayReturnBucketTypeDoesNotExist 'UnsetBucketIndexOp    = 'True
  MayReturnBucketTypeDoesNotExist 'UpdateCrdtOp          = 'True
  MayReturnBucketTypeDoesNotExist _                      = 'False

type family MayReturnIndexDoesNotExist (op :: Op) :: Bool where
  MayReturnBucketTypeDoesNotExist 'SearchOp             = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketIndexOp     = 'True
  MayReturnBucketTypeDoesNotExist 'SetBucketTypeIndexOp = 'True
  MayReturnBucketTypeDoesNotExist _                     = 'False

type family MayReturnIndexHasAssociatedBuckets  (op :: Op) :: Bool where
  MayReturnIndexHasAssociatedBuckets 'DeleteIndexOp = 'True
  MayReturnIndexHasAssociatedBuckets 'PutIndexOp    = 'True
  MayReturnIndexHasAssociatedBuckets _              = 'False

type family MayReturnInvalidBucket (op :: Op) :: Bool where
  MayReturnInvalidBucket 'PutOp        = 'True
  MayReturnInvalidBucket 'UpdateCrdtOp = 'True
  MayReturnInvalidBucket _             = 'False

type family MayReturnInvalidBucketType (op :: Op) :: Bool where
  MayReturnInvalidBucketType 'GetBucketOp          = 'True
  MayReturnInvalidBucketType 'SetBucketTypeIndexOp = 'True
  MayReturnInvalidBucketType 'UpdateCrdtOp         = 'True
  MayReturnInvalidBucketType _                     = 'False

type family MayReturnInvalidKey (op :: Op) :: Bool where
  MayReturnInvalidKey 'GetOp = 'True
  MayReturnInvalidKey _      = 'False

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

isAllNodesDownError :: ByteString -> Bool
isAllNodesDownError =
  (== "all_nodes_down")

isBucketCannotBeZeroLengthError :: ByteString -> Bool
isBucketCannotBeZeroLengthError =
  (== "Bucket cannot be zero-length")

isBucketMustBeAllowMultError :: ByteString -> Bool
isBucketMustBeAllowMultError =
  (== "\"Bucket must be allow_mult=true\"")

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

isBucketTypeDoesNotExistError5 :: ByteString -> Bool
isBucketTypeDoesNotExistError5 =
  isRight . Atto.parseOnly parser

  where
    parser :: Atto.Parser ()
    parser = do
      Atto.string "Error processing incoming message: error:{badmatch," *> Atto.skipSpace
      Atto.string "{error," *> Atto.skipSpace
      Atto.string "{function_clause," *> Atto.skipSpace
      Atto.string "[{proplists," *> Atto.skipSpace
      Atto.string "get_value," *> Atto.skipSpace
      Atto.string "[n_val," *> Atto.skipSpace
      Atto.string "{error," *> Atto.skipSpace
      Atto.string "no_type}" $> ()

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

isInsufficientVnodesError0 :: ByteString -> Bool
isInsufficientVnodesError0 =
  (== "{error,insufficient_vnodes_available}")

isInsufficientVnodesError1 :: ByteString -> Bool
isInsufficientVnodesError1 =
  ByteString.isPrefixOf "{insufficient_vnodes"

isInvalidCounterBucketError :: ByteString -> Bool
isInvalidCounterBucketError =
  ByteString.isPrefixOf "\"Counters require bucket property"

isInvalidNodesError0 :: ByteString -> Bool
isInvalidNodesError0 =
  ByteString.isPrefixOf "{n_val_violation"

isInvalidNodesError1 :: ByteString -> Bool
isInvalidNodesError1 =
  ByteString.isPrefixOf "Invalid bucket properties: [{n_val,"

isInvalidSchemaError :: ByteString -> Bool
isInvalidSchemaError =
  ByteString.isPrefixOf "Error storing schema"

isKeyCannotBeZeroLengthError :: ByteString -> Bool
isKeyCannotBeZeroLengthError =
  (== "Key cannot be zero-length")

isNonCounterOperationOnDefaultBucketError :: ByteString -> Bool
isNonCounterOperationOnDefaultBucketError =
  (== "\"non-counter operation on default bucket\"")

isNotfoundError :: ByteString -> Bool
isNotfoundError =
  (== "notfound")

isOperationTypeIsCounterButBucketTypeIsError :: ByteString -> Bool
isOperationTypeIsCounterButBucketTypeIsError =
  ByteString.isPrefixOf "Operation type is `counter` but  bucket type is"

isOperationTypeIsHllButBucketTypeIsError :: ByteString -> Bool
isOperationTypeIsHllButBucketTypeIsError =
  ByteString.isPrefixOf "Operation type is `hll` but  bucket type is"

isOperationTypeIsMapButBucketTypeIsError :: ByteString -> Bool
isOperationTypeIsMapButBucketTypeIsError =
  ByteString.isPrefixOf "Operation type is `map` but  bucket type is"

isOperationTypeIsSetButBucketTypeIsError :: ByteString -> Bool
isOperationTypeIsSetButBucketTypeIsError =
  ByteString.isPrefixOf "Operation type is `set` but  bucket type is"

isOverloadError :: ByteString -> Bool
isOverloadError =
  (== "overload")

isPrValUnsatisfied :: ByteString -> Bool
isPrValUnsatisfied =
  ByteString.isPrefixOf "{pr_val_unsatisfied"

isPwValUnsatisfied :: ByteString -> Bool
isPwValUnsatisfied =
  ByteString.isPrefixOf "{pw_val_unsatisfied"

isSchemaDoesNotExistError :: ByteString -> Bool
isSchemaDoesNotExistError =
  (== "Schema not found")

isSearchFailedError :: ByteString -> Bool
isSearchFailedError =
  (== "Query unsuccessful check the logs.")

isSecondaryIndexesNotSupportedError :: ByteString -> Bool
isSecondaryIndexesNotSupportedError =
  ByteString.isPrefixOf "{error,{indexes_not_supported,"

isUnknownMessageCodeError :: ByteString -> Bool
isUnknownMessageCodeError =
  ByteString.isPrefixOf "Unknown message code:"
