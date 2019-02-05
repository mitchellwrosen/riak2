module Riak.Schema
  ( Schema(..)
  , get
  , put
  ) where

import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L


data Schema
  = Schema
  { name :: !Text
  , content :: !ByteString -- TODO schema contents are Text?
  }

get ::
     MonadIO m
  => Client
  -> Text
  -> m (Either Text (Maybe Schema))
get client name = liftIO $
  fromResponse <$>
    Client.exchange
      client
      (RequestGetSchema request)
      (\case
        ResponseGetSchema response -> Just response
        _ -> Nothing)

  where
    request :: Proto.GetSchemaRequest
    request =
      defMessage
        & L.name .~ encodeUtf8 name

    fromResponse ::
         Either Text Proto.GetSchemaResponse
      -> Either Text (Maybe Schema)
    fromResponse = \case
      -- TODO text "notfound" string
      Left err ->
        Left err

      Right response ->
        Right (Just (fromProto (response ^. L.schema)))

put ::
     MonadIO m
  => Client
  -> Schema
  -> m (Either Text ())
put client schema = liftIO $
  Client.exchange
    client
    (RequestPutSchema request)
    (\case
      ResponsePut{} -> Just ()
      _ -> Nothing)

  where
    request :: Proto.PutSchemaRequest
    request =
      defMessage
        & L.schema .~ toProto schema

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
