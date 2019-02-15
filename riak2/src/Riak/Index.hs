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
  fromResponse <$>
    Handle.getIndex handle (Just (encodeUtf8 name))

  where
    fromResponse ::
         Either Handle.Error [Proto.RpbYokozunaIndex]
      -> Either (Error 'GetIndexOp) (Maybe Index)
    fromResponse = \case
      Left (Handle.ErrorHandle err) ->
        Left (HandleError err)

      Left (Handle.ErrorRiak err)
        | isNotfound err ->
            Right Nothing
        | isUnknownMessageCode err ->
            Left SearchNotEnabledError
        | otherwise ->
            Left (UnknownError (decodeUtf8 err))

      Right (head -> index) ->
        Right (Just (fromProto index))

-- | Get all Solr indexes.
getIndexes ::
     MonadIO m
  => Handle -- ^
  -> m (Either (Error 'GetIndexOp) [Index])
getIndexes handle = liftIO $
  fromResponse <$> Handle.getIndex handle Nothing

  where
    fromResponse ::
         Either Handle.Error [Proto.RpbYokozunaIndex]
      -> Either (Error 'GetIndexOp) [Index]
    fromResponse = \case
      Left (Handle.ErrorHandle err) ->
        Left (HandleError err)

      Left (Handle.ErrorRiak err)
        | isUnknownMessageCode err ->
            Left SearchNotEnabledError
        | otherwise ->
            Left (UnknownError (decodeUtf8 err))

      Right indexes ->
        Right (map fromProto indexes)

-- | Put a Solr index.
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
    then pure (Left (InvalidNodesError 0))
    else liftIO (putIndex_ handle index schema nodes timeout)

putIndex_ ::
     Handle
  -> IndexName
  -> Text
  -> Maybe Natural
  -> Maybe Word32
  -> IO (Either (Error 'PutIndexOp) ())
putIndex_ handle index schema nodes timeout =
  first parsePutIndexError <$> Handle.putIndex handle request

  where
    request :: Proto.RpbYokozunaIndexPutReq
    request =
      Proto.defMessage
        & Proto.index .~
            (Proto.defMessage
              & Proto.name .~ encodeUtf8 (unIndexName index)
              & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
              & Proto.schema .~ encodeUtf8 schema)
        & Proto.maybe'timeout .~ timeout

    parsePutIndexError :: Handle.Error -> Error 'PutIndexOp
    parsePutIndexError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isSchemaDoesNotExistError err ->
            SchemaDoesNotExistError
        | isUnknownMessageCode err ->
            SearchNotEnabledError
        | otherwise ->
            UnknownError (decodeUtf8 err)

-- | Delete a Solr index.
--
-- This function swallows @"notfound"@ errors from Riak thrown when you try to
-- delete an index that doesn't exist. Feedback welcome on this decision.
deleteIndex ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^
  -> m (Either (Error 'DeleteIndexOp) ())
deleteIndex handle name = liftIO $
  fromResponse <$>
    Handle.deleteIndex handle (encodeUtf8 (unIndexName name))

  where
    fromResponse :: Either Handle.Error () -> Either (Error 'DeleteIndexOp) ()
    fromResponse = \case
      Left (Handle.ErrorHandle err) ->
        Left (HandleError err)

      Left (Handle.ErrorRiak err)
        | isNotfound err ->
            Right ()
        | isUnknownMessageCode err ->
            Left SearchNotEnabledError
        | otherwise ->
            Left (UnknownError (decodeUtf8 err))

      Right () ->
        Right ()

fromProto :: Proto.RpbYokozunaIndex -> Index
fromProto index =
  Index
    { name = IndexName (decodeUtf8 (index ^. Proto.name))
    , nodes = fromIntegral (index ^. Proto.nVal)
    , schema = decodeUtf8 (index ^. Proto.schema)
    }
