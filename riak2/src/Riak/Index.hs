module Riak.Index
  ( Index(..)
  , getIndex
  , getIndexes
  , putIndex
  , deleteIndex
  ) where

import Riak.Client           (Client)
import Riak.Internal.Prelude

import qualified Riak.Interface as Interface

import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Data.List (head)

-- | A Solr index.
--
-- /Note/. The index name may only contain ASCII values from @32-127@.
data Index
  = Index
  { name :: Text
  , schema :: Text
  , n :: Maybe Word32 -- TODO does riak always return this?
  }

-- | Get a Solr index.
getIndex ::
     MonadIO m
  => Client
  -> Text
  -> m (Either ByteString (Maybe Index))
getIndex client name = liftIO $
  liftIO (fromResponse <$> Interface.getIndex client (Just (encodeUtf8 name)))

  where
    fromResponse ::
         Either ByteString [Proto.Index]
      -> Either ByteString (Maybe Index)
    fromResponse = \case
      -- TODO test that riak returns "notfound" here instead of an empty list
      -- Left "notfound" ->
      --   Right Nothing

      Left err ->
        Left err

      Right (head -> index) ->
        Right $ Just $ Index
          { name = name
          , schema = decodeUtf8 (index ^. L.schema)
          , n = index ^. L.maybe'n
          }

-- | Get all Solr indexes.
getIndexes ::
     MonadIO m
  => Client
  -> m (Either ByteString [Index])
getIndexes client =
  liftIO (fromResponse <$> Interface.getIndex client Nothing)

  where
    fromResponse ::
         Either ByteString [Proto.Index]
      -> Either ByteString [Index]
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
  => Client -- ^
  -> Index -- ^
  -> m (Either ByteString ())
putIndex client index = liftIO $
  Interface.putIndex client request

  where
    request :: Proto.PutIndexRequest
    request =
      defMessage
        & L.index .~ toProto index
        -- TODO put index timeout
        -- & L.maybe'timeout .~ undefined

-- | Delete a Solr index.
deleteIndex ::
     MonadIO m
  => Client -- ^
  -> Text -- ^
  -> m (Either ByteString ())
deleteIndex client name = liftIO $
  Interface.deleteIndex client (encodeUtf8 name)

fromProto :: Proto.Index -> Index
fromProto index =
  Index
    { name = decodeUtf8 (index ^. L.name)
    , schema = decodeUtf8 (index ^. L.schema)
    , n = index ^. L.maybe'n
    }

toProto :: Index -> Proto.Index
toProto Index { name, schema, n } =
  defMessage
    & L.name .~ encodeUtf8 name
    & L.schema .~ encodeUtf8 schema
    & L.maybe'n .~ n
