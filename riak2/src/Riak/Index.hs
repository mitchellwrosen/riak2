module Riak.Index
  ( -- * Index
    getIndex
  , getIndexes
  , putIndex
  , PutIndexOpts(..)
  , deleteIndex
  , Index(..)
    -- * Index name
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  ) where

import Libriak.Handle          (Handle)
import Riak.Internal.Error
import Riak.Internal.IndexName
import Riak.Internal.Prelude
import Riak.Schema             (defaultSchema)

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

import Control.Lens       ((.~), (^.))
import Data.Default.Class (Default(..))
import Data.List          (head)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | A Solr index.
--
-- @nodes@ is necessary for query plan calculation, and indicates the number of
-- nodes that each bucket with which this index is associated with is replicated
-- to.
--
-- /See also/: Riak.BucketType.setBucketTypeIndex
data Index
  = Index
  { name :: !IndexName
  , nodes :: !Natural
  , schema :: !Text
  } deriving stock (Generic, Show)

data PutIndexOpts
  = PutIndexOpts
  { nodes :: !(Maybe Natural) -- ^ Must be positive
  , timeout :: !(Maybe Word32)
  } deriving stock (Generic, Show)

instance Default PutIndexOpts where
  def :: PutIndexOpts
  def =
    PutIndexOpts
      { nodes = Nothing
      , timeout = Nothing
      }


-- | Get a Solr index.
getIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^
  -> m (Either (Error 'GetIndexOp) (Maybe Index))
getIndex handle (IndexName name) = liftIO $
  fromHandleResult
    parseGetIndexError
    (Just . fromProto . head)
    (Handle.getIndex handle (Just (encodeUtf8 name)))

parseGetIndexError ::
     ByteString
  -> Either (Error 'GetIndexOp) (Maybe Index)
parseGetIndexError err
  | isNotfound err =
      Right Nothing
  | isUnknownMessageCode err =
      Left SearchNotEnabledError
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Get all Solr indexes.
getIndexes ::
     MonadIO m
  => Handle -- ^
  -> m (Either (Error 'GetIndexOp) [Index])
getIndexes handle = liftIO $
  fromHandleResult
    (Left . parseGetIndexesError)
    (map fromProto)
    (Handle.getIndex handle Nothing)

parseGetIndexesError :: ByteString -> Error 'GetIndexOp
parseGetIndexesError err
  | isUnknownMessageCode err =
      SearchNotEnabledError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | Put a Solr index.
--
-- /Note/: If you attempt to put an index that already exists, even if you
-- supply a different @nodes@ or @schema@ value, Riak will ignore the put but
-- not return an error.
--
-- /See also/: 'Riak.Schema.defaultSchema'
putIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^ Index name
  -> Text -- ^ Schema name
  -> PutIndexOpts -- ^
  -> m (Either (Error 'PutIndexOp) ())
putIndex handle index schema (PutIndexOpts nodes timeout) =
  if nodes == Just 0
    then pure (Left InvalidNodesError)
    else liftIO (putIndex_ handle index schema nodes timeout)

putIndex_ ::
     Handle
  -> IndexName
  -> Text
  -> Maybe Natural
  -> Maybe Word32
  -> IO (Either (Error 'PutIndexOp) ())
putIndex_ handle index schema nodes timeout =
  fromHandleResult
    (Left . parsePutIndexError)
    id
    (Handle.putIndex handle request)

  where
    request :: Proto.RpbYokozunaIndexPutReq
    request =
      Proto.defMessage
        & Proto.index .~
            (Proto.defMessage
              & Proto.name .~ encodeUtf8 (unIndexName index)
              & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
              & Proto.maybe'schema .~
                  (if schema == defaultSchema
                    then Nothing
                    else Just (encodeUtf8 schema)))
        & Proto.maybe'timeout .~ timeout

parsePutIndexError :: ByteString -> Error 'PutIndexOp
parsePutIndexError err
  | isSchemaDoesNotExistError err =
      SchemaDoesNotExistError
  | isUnknownMessageCode err =
      SearchNotEnabledError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | Delete a Solr index. Returns whether or not there was an index to delete.
--
-- TODO Parse this delete index error:
-- Can't delete index with associate buckets [{<<\"objects\">>,<<\"foo\">>},\n    {<<\"objects\">>,<<\"bar\">>}]
deleteIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^
  -> m (Either (Error 'DeleteIndexOp) Bool)
deleteIndex handle name = liftIO $
  fromHandleResult
    parseDeleteIndexError
    (\() -> True)
    (Handle.deleteIndex handle (encodeUtf8 (unIndexName name)))

parseDeleteIndexError ::
     ByteString
  -> Either (Error 'DeleteIndexOp) Bool
parseDeleteIndexError err
  | isNotfound err =
      Right False
  | isUnknownMessageCode err =
      Left SearchNotEnabledError
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

fromProto :: Proto.RpbYokozunaIndex -> Index
fromProto index =
  Index
    { name = IndexName (decodeUtf8 (index ^. Proto.name))
    , nodes = fromIntegral (index ^. Proto.nVal)
    , schema = decodeUtf8 (index ^. Proto.schema)
    }
