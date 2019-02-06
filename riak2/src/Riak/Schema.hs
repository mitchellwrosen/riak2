module Riak.Schema
  ( Schema(..)
  , getSchema
  , putSchema
  ) where

import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude

import qualified Riak.Interface  as Interface
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L


-- | A Solr schema.
data Schema
  = Schema
  { name :: !Text
  , content :: !ByteString -- TODO schema contents are Text?
  }

-- | Put a Solr schema.
getSchema ::
     MonadIO m
  => Client -- ^
  -> Text -- ^
  -> m (Either ByteString (Maybe Schema))
getSchema client name = liftIO $
  fromResponse <$>
    Interface.getSchema
      client
      (encodeUtf8 name)

  where
    fromResponse ::
         Either ByteString Proto.Schema
      -> Either ByteString (Maybe Schema)
    fromResponse = \case
      -- TODO text "notfound" string
      Left err ->
        Left err

      Right schema ->
        Right (Just (fromProto schema))

-- | Get a Solr schema.
putSchema ::
     MonadIO m
  => Client -- ^
  -> Schema -- ^
  -> m (Either ByteString ())
putSchema client schema =
  liftIO (Interface.putSchema client (toProto schema))

fromProto :: Proto.Schema -> Schema
fromProto schema =
  Schema
    { name = decodeUtf8 (schema ^. L.name)
    , content = schema ^. L.content
    }

toProto :: Schema -> Proto.Schema
toProto Schema { name, content } =
  defMessage
    & L.content .~ content
    & L.name .~ encodeUtf8 name
