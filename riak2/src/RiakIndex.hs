module RiakIndex where

import Libriak.Response (Response(..))
import RiakError
import RiakHandle       (Handle)
import RiakIndexName
import RiakSchema       (defaultSchema)

import qualified Libriak.Proto as Proto
import qualified RiakHandle    as Handle

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
  -> m (Either GetIndexError (Maybe Index))
getIndex handle (IndexName name) = liftIO $
  Handle.getIndex handle (Just (encodeUtf8 name)) >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (parseGetIndexError err)

    Right (Right (RespRpbYokozunaIndexGet response)) ->
      pure (Right (Just (fromProto (head (response ^. Proto.index)))))

parseGetIndexError ::
     ByteString
  -> Either GetIndexError (Maybe Index)
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
  -> m (Either GetIndexError [Index])
getIndexes handle = liftIO $
  Handle.getIndex handle Nothing >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseGetIndexesError err))

    Right (Right (RespRpbYokozunaIndexGet response)) ->
      pure (Right (map fromProto (response ^. Proto.index)))

parseGetIndexesError :: ByteString -> GetIndexError
parseGetIndexesError err
  | isUnknownMessageCode err =
      SearchNotEnabledError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | Put a Solr index.
--
-- /See also/: 'Riak.Schema.defaultSchema'
putIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^ Index name
  -> Text -- ^ Schema name
  -> PutIndexOpts -- ^
  -> m (Either PutIndexError ())
putIndex handle index schema (PutIndexOpts nodes timeout) =
  if nodes == Just 0
    then pure (Left InvalidNodesError)
    else liftIO (putIndex_ handle index schema nodes timeout)

-- Riak is too dumb to modify an index that already exists. Instead, have to
-- delete and re-put.
putIndex_ ::
     Handle
  -> IndexName
  -> Text
  -> Maybe Natural
  -> Maybe Word32
  -> IO (Either PutIndexError ())
putIndex_ handle index schema nodes timeout = do
  getIndex handle index >>= \case
    Left err ->
      case err of
        SearchNotEnabledError -> pure (Left SearchNotEnabledError)
        HandleError err' -> pure (Left (HandleError err'))
        UnknownError err' -> pure (Left (UnknownError err'))

    Right Nothing ->
      doPutIndex

    Right (Just (Index _ oldNodes oldSchema)) ->
      -- Modify: delete then put
      if nodes /= Just oldNodes || schema /= oldSchema
        then
          deleteIndex handle index >>= \case
            Left err ->
              case err of
                SearchNotEnabledError -> pure (Left SearchNotEnabledError)
                HandleError err' -> pure (Left (HandleError err'))
                UnknownError err' -> pure (Left (UnknownError err'))

            Right _success ->
              doPutIndex

        -- Same as before: no need to modify
        else
          pure (Right ())

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

    doPutIndex =
      Handle.putIndex handle request >>= \case
        Left err ->
          pure (Left (HandleError err))

        Right (Left err) ->
          pure (Left (parsePutIndexError err))

        Right (Right _) ->
          pure (Right ())

parsePutIndexError :: ByteString -> PutIndexError
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
  -> m (Either DeleteIndexError Bool)
deleteIndex handle name = liftIO $
  Handle.deleteIndex handle (encodeUtf8 (unIndexName name)) >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (parseDeleteIndexError err)

    Right (Right _) ->
      pure (Right True)

parseDeleteIndexError ::
     ByteString
  -> Either DeleteIndexError Bool
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
