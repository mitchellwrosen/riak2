module RiakIndex
  ( Index(..)
  , getIndex
  , getIndexes
  , putIndex
  , PutIndexOpts(..)
  , deleteIndex
  ) where

import RiakError
import RiakHandle      (Handle)
import RiakHandleError (HandleError)
import RiakIndexName
import RiakSchema      (defaultSchema)

import qualified RiakHandle as Handle

import Control.Lens       ((.~), (^.))
import Data.Default.Class (Default(..))
import Data.List          (head)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.Riak.Proto as Proto


-- | A Solr index.
--
-- @nodes@ is necessary for query plan calculation, and indicates the number of
-- nodes that each bucket with which this index is associated with is replicated
-- to.
--
-- /See also/: Riak.BucketType.setBucketTypeIndex
data Index
  = Index
  { name :: IndexName
  , nodes :: Natural
  , schema :: Text
  } deriving stock (Generic, Show)

data PutIndexOpts
  = PutIndexOpts
  { nodes :: Maybe Natural -- ^ Must be positive
  , timeout :: Maybe Word32
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

    Right (Right response) ->
      pure (Right (Just (fromProto (head (response ^. Proto.index)))))

parseGetIndexError ::
     ByteString
  -> Either GetIndexError (Maybe Index)
parseGetIndexError err
  | isNotfoundError err =
      Right Nothing
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Get all Solr indexes.
getIndexes ::
     MonadIO m
  => Handle -- ^
  -> m (Either GetIndexError [Index])
getIndexes handle = liftIO $
  fromResult <$> Handle.getIndex handle Nothing

  where
    fromResult ::
         Either [HandleError] (Either ByteString Proto.RpbYokozunaIndexGetResp)
      -> Either GetIndexError [Index]
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseGetIndexesError err)

      Right (Right response) ->
        Right (map fromProto (response ^. Proto.index))

parseGetIndexesError :: ByteString -> GetIndexError
parseGetIndexesError err =
  UnknownError (decodeUtf8 err)

-- TODO putIndexWith?

-- | Put a Solr index.
--
-- +----------------------------------+----------------------------------------+
-- | Error                            | Meaning                                |
-- +==================================+========================================+
-- | 'IndexHasAssociatedBucketsError' | A put was attempted that would change  |
-- |                                  | options of an existing index. Rather   |
-- |                                  | than silently ignore the request as    |
-- |                                  | Riak would, this library attempts to   |
-- |                                  | delete and recreate the index.         |
-- |                                  | However, an index may only be deleted  |
-- |                                  | if it is not associated with any       |
-- |                                  | buckets. The buckets must first each   |
-- |                                  | be unassociated using                  |
-- |                                  | 'Riak.Bucket.unsetSearchIndex'.        |
-- +----------------------------------+----------------------------------------+
-- | 'SchemaDoesNotExistError'        | The schema does not exist. You must    |
-- |                                  | first create it with                   |
-- |                                  | 'Riak.Schema.putSchema', or use the    |
-- |                                  | 'Riak.Schema.defaultSchema'.           |
-- +----------------------------------+----------------------------------------+
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
              pure (Left (fromDeleteIndexError err))

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
              & Proto.name .~ encodeUtf8 (_unIndexName index)
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

    fromDeleteIndexError :: DeleteIndexError -> PutIndexError
    fromDeleteIndexError = \case
      HandleError err ->
        HandleError err
      IndexHasAssociatedBucketsError name buckets ->
        IndexHasAssociatedBucketsError name buckets
      UnknownError err ->
        UnknownError err

parsePutIndexError :: ByteString -> PutIndexError
parsePutIndexError err
  | isSchemaDoesNotExistError err =
      SchemaDoesNotExistError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | Delete a Solr index. Returns whether or not there was an index to delete.
--
-- +----------------------------------+----------------------------------------+
-- | Error                            | Meaning                                |
-- +==================================+========================================+
-- | 'IndexHasAssociatedBucketsError' | A delete was attempted on an index     |
-- |                                  | with associated buckets. The buckets   |
-- |                                  | must first each be unassociated using  |
-- |                                  | 'Riak.Bucket.unsetBucketIndex'.        |
-- +----------------------------------+----------------------------------------+
deleteIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^
  -> m (Either DeleteIndexError Bool)
deleteIndex handle name = liftIO $
  fromResult <$> Handle.deleteIndex handle (encodeUtf8 (_unIndexName name))

  where
    fromResult ::
         Either [HandleError] (Either ByteString ())
      -> Either DeleteIndexError Bool
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        parseDeleteIndexError name err

      Right (Right _) ->
        Right True

parseDeleteIndexError ::
     IndexName
  -> ByteString
  -> Either DeleteIndexError Bool
parseDeleteIndexError name err
  | Just buckets <- isHasAssociatedBucketsError err =
      Left (IndexHasAssociatedBucketsError name buckets)
  | isNotfoundError err =
      Right False
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

fromProto :: Proto.RpbYokozunaIndex -> Index
fromProto index =
  Index
    { name = IndexName (decodeUtf8 (index ^. Proto.name))
    , nodes = fromIntegral (index ^. Proto.nVal)
    , schema = decodeUtf8 (index ^. Proto.schema)
    }
