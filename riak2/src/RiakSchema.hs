module RiakSchema
  ( Schema(..)
  , defaultSchema
  , getSchema
  , putSchema
  ) where

import Libriak.Response (Response(..))
import RiakError
import RiakHandle       (Handle)

import qualified RiakHandle as Handle

import Control.Lens       ((.~), (^.))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.Riak.Proto as Proto


-- | A Solr schema.
data Schema
  = Schema
  { name :: !Text
  , content :: !ByteString
  } deriving stock (Generic, Show)

-- | The default search schema @"_yz_default"@.
defaultSchema :: Text
defaultSchema =
  "_yz_default"

-- | Put a Solr schema.
getSchema ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^
  -> m (Either GetSchemaError (Maybe Schema))
getSchema handle name = liftIO $
  Handle.getSchema handle (encodeUtf8 name) >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (parseGetSchemaError err)

    Right (Right response) ->
      pure (Right (Just (fromProto (response ^. Proto.schema))))

parseGetSchemaError ::
     ByteString
  -> Either GetSchemaError (Maybe Schema)
parseGetSchemaError err
  | isNotfound err =
      Right Nothing
  | isUnknownMessageCode err =
      Left SearchNotEnabledError
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Get a Solr schema.
putSchema ::
     MonadIO m
  => Handle -- ^
  -> Schema -- ^
  -> m (Either PutSchemaError ())
putSchema handle schema = liftIO $
  Handle.putSchema handle (toProto schema) >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parsePutSchemaError err))

    Right (Right _) ->
      pure (Right ())

parsePutSchemaError ::
     ByteString
  -> PutSchemaError
parsePutSchemaError err
  | isInvalidSchemaError err =
      InvalidSchemaError (decodeUtf8 err)
  | isUnknownMessageCode err =
      SearchNotEnabledError
  | otherwise =
      UnknownError (decodeUtf8 err)

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
