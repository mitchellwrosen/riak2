module Riak.Index
  ( Index(..)
  , getIndex
  , getIndexes
  , putIndex
  , deleteIndex
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude

import qualified Libriak.Handle as Handle

import qualified Libriak.Proto as Proto

import Control.Lens       ((.~), (^.))
import Data.List          (head)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | A Solr index.
--
-- /Note/. The index name may only contain ASCII values from @32-127@.
data Index
  = Index
  { name :: !Text
  , nodes :: Maybe Word32 -- TODO does riak always return this?
  , schema :: !Text
  }

-- | Get a Solr index.
getIndex ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^
  -> m (Either Handle.Error (Maybe Index))
getIndex handle name = liftIO $
  liftIO (fromResponse <$> Handle.getIndex handle (Just (encodeUtf8 name)))

  where
    fromResponse ::
         Either Handle.Error [Proto.RpbYokozunaIndex]
      -> Either Handle.Error (Maybe Index)
    fromResponse = \case
      -- TODO test that riak returns "notfound" here instead of an empty list
      -- Left "notfound" ->
      --   Right Nothing

      Left err ->
        Left err

      Right (head -> index) ->
        Right $ Just $ Index
          { name = name
          , nodes = index ^. Proto.maybe'nVal
          , schema = decodeUtf8 (index ^. Proto.schema)
          }

-- | Get all Solr indexes.
getIndexes ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.Error [Index])
getIndexes handle =
  liftIO (fromResponse <$> Handle.getIndex handle Nothing)

  where
    fromResponse ::
         Either Handle.Error [Proto.RpbYokozunaIndex]
      -> Either Handle.Error [Index]
    fromResponse = \case
      -- TODO test that riak returns "notfound" here instead of an empty list
      -- Left "notfound" ->
      --   Right []
      Left err ->
        Left err

      Right indexes ->
        Right (map fromProto indexes)

-- | Put a Solr index.
putIndex ::
     MonadIO m
  => Handle -- ^
  -> Index -- ^
  -> m (Either Handle.Error ())
putIndex handle index = liftIO $
  Handle.putIndex handle request

  where
    request :: Proto.RpbYokozunaIndexPutReq
    request =
      Proto.defMessage
        & Proto.index .~ toProto index
        -- TODO put index timeout
        -- & Proto.maybe'timeout .~ undefined

-- | Delete a Solr index.
deleteIndex ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^
  -> m (Either Handle.Error ())
deleteIndex handle name = liftIO $
  Handle.deleteIndex handle (encodeUtf8 name)

fromProto :: Proto.RpbYokozunaIndex -> Index
fromProto index =
  Index
    { name = decodeUtf8 (index ^. Proto.name)
    , nodes = index ^. Proto.maybe'nVal
    , schema = decodeUtf8 (index ^. Proto.schema)
    }

toProto :: Index -> Proto.RpbYokozunaIndex
toProto Index { name, nodes, schema } =
  Proto.defMessage
    & Proto.maybe'nVal .~ nodes
    & Proto.name .~ encodeUtf8 name
    & Proto.schema .~ encodeUtf8 schema
