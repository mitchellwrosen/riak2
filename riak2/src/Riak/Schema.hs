module Riak.Schema
  ( Schema(..)
  , getSchema
  , putSchema
  ) where

import Riak.Handle           (Handle)
import Riak.Internal.Prelude

import qualified Riak.Handle     as Handle
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Control.Lens       ((.~), (^.))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | A Solr schema.
data Schema
  = Schema
  { name :: !Text
  , content :: !ByteString -- TODO schema contents are Text?
  }

-- | Put a Solr schema.
getSchema ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^
  -> m (Either ByteString (Maybe Schema))
getSchema handle name = liftIO $
  fromResponse <$>
    Handle.getSchema
      handle
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
  => Handle -- ^
  -> Schema -- ^
  -> m (Either ByteString ())
putSchema handle schema =
  liftIO (Handle.putSchema handle (toProto schema))

fromProto :: Proto.Schema -> Schema
fromProto schema =
  Schema
    { name = decodeUtf8 (schema ^. L.name)
    , content = schema ^. L.content
    }

toProto :: Schema -> Proto.Schema
toProto Schema { name, content } =
  Proto.defMessage
    & L.content .~ content
    & L.name .~ encodeUtf8 name
