module Riak.Schema
  ( Schema(..)
  , defaultSchema
  , getSchema
  , putSchema
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

import Control.Lens       ((.~), (^.))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | A Solr schema.
data Schema
  = Schema
  { name :: !Text
  , content :: !ByteString -- TODO schema contents are Text?
  }

-- | The default search schema @"_yz_default"@.
defaultSchema :: Text
defaultSchema =
  "_yz_default"

-- | Put a Solr schema.
getSchema ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^
  -> m (Either Handle.Error (Maybe Schema))
getSchema handle name = liftIO $
  fromResponse <$>
    Handle.getSchema
      handle
      (encodeUtf8 name)

  where
    fromResponse ::
         Either Handle.Error Proto.RpbYokozunaSchema
      -> Either Handle.Error (Maybe Schema)
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
  -> m (Either Handle.Error ())
putSchema handle schema =
  liftIO (Handle.putSchema handle (toProto schema))

fromProto :: Proto.RpbYokozunaSchema -> Schema
fromProto schema =
  Schema
    { name = decodeUtf8 (schema ^. Proto.name)
    , content = schema ^. Proto.content
    }

toProto :: Schema -> Proto.RpbYokozunaSchema
toProto Schema { name, content } =
  Proto.defMessage
    & Proto.content .~ content
    & Proto.name .~ encodeUtf8 name
