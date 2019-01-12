{-# LANGUAGE UndecidableInstances #-}

module Riak.Internal.Object
  ( Charset(..)
  , RiakObject(..)
  , ContentEncoding(..)
  , ContentType(..)
  , IsRiakObject(..)
  , JsonRiakObject(..)
  ) where

import Data.Aeson             (FromJSON, ToJSON)
import Data.Bifunctor         (first)
import Data.Time

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding   as Text

import Riak.Internal.Prelude
import Riak.Internal.Types

data RiakObject a
  = RiakObject
  { key :: !RiakKey
  , value :: a
  , contentType :: Maybe ContentType
  , charset :: Maybe Charset
  , contentEncoding :: Maybe ContentEncoding
  , vtag :: Maybe RiakVtag
  , lastModified :: Maybe UTCTime
  , metadata :: RiakMetadata
  , indexes :: [RiakIndex]
  , deleted :: Bool
  , ttl :: TTL
  } deriving stock (Eq, Functor, Generic, Show)


-- | 'IsRiakObject' classifies types that are stored in Riak objects. Every
-- object /should/ have a content type, /may/ have a character set and /may/
-- have an encoding.
--
-- For convenience, instances are provided by this library to store binary,
-- textual, and JSON data.
--
-- +-----------------------------+----------------------------+------------------------------+------------------------------+
-- |                             | Content type               | Charset                      | Content encoding             |
-- +==================+==========+============================+==============================+==============================+
-- | __@ByteString@__ | On read  | /ignored/                  | /ignored/                    | /ignored/                    |
-- |                  +----------+----------------------------+------------------------------+------------------------------+
-- |                  | On write | @application/octet-stream@ | /empty/                      | /empty/                      |
-- +------------------+----------+----------------------------+------------------------------+------------------------------+
-- | __@Text@__       | On read  | @text/plain@               | /empty/, @ascii@, or @utf-8@ | /empty/, @ascii@, or @utf-8@ |
-- |                  +----------+----------------------------+------------------------------+------------------------------+
-- |                  | On write | @text/plain@               | @utf-8@                      | @utf-8@                      |
-- +------------------+----------+----------------------------+------------------------------+------------------------------+
-- | __@JSON@__       | On read  | @application/json@         | /ignored/                    | /ignored/                    |
-- |                  +----------+----------------------------+------------------------------+------------------------------+
-- |                  | On write | @application/json@         | /empty/                      | /empty/                      |
-- +------------------+----------+----------------------------+------------------------------+------------------------------+
--
-- The @ByteString@ instance allows you to read any Riak object's raw bytes,
-- regardless of its type, as it ignores the content type, charset, and
-- encoding.
--
-- JSON data can be stored using a 'Aeson.Value' or the 'JsonRiakObject'
-- newtype wrapper, which can be used in a @deriving via@ clause as e.g.
--
-- @
-- data MyType = MyType ...
--   deriving 'IsRiakObject' via 'JsonRiakObject'
-- @
--
-- When writing your own 'IsRiakObject' instances, use any content type,
-- charset, and content encoding you wish. None of them are actually required by
-- Riak, they are for you to help make sense of your own data.
class IsRiakObject a where
  -- | The object's content type.
  riakObjectContentType :: a -> Maybe ContentType

  -- | The object's character set. Defaults to 'Nothing'.
  riakObjectCharset :: a -> Maybe Charset
  riakObjectCharset _ =
    Nothing

  -- | The object's encoding. Defaults to 'Nothing'.
  riakObjectContentEncoding :: a -> Maybe ContentEncoding
  riakObjectContentEncoding _ =
    Nothing

  -- | Encode a Riak object.
  encodeRiakObject :: a -> ByteString

  -- | Decode a Riak object, given its content type, charset, encoding, and
  -- encoded value.
  decodeRiakObject
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException a

instance IsRiakObject ByteString where
  riakObjectContentType :: ByteString -> Maybe ContentType
  riakObjectContentType _ =
    Just (ContentType "application/octet-stream")

  encodeRiakObject :: ByteString -> ByteString
  encodeRiakObject =
    id

  decodeRiakObject
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException ByteString
  decodeRiakObject _ _ _ bytes =
    Right bytes

instance IsRiakObject Text where
  riakObjectContentType :: Text -> Maybe ContentType
  riakObjectContentType _ =
    Just (ContentType "text/plain")

  riakObjectCharset :: Text -> Maybe Charset
  riakObjectCharset _ =
    Just (Charset "utf-8")

  riakObjectContentEncoding :: Text -> Maybe ContentEncoding
  riakObjectContentEncoding _ =
    Just (ContentEncoding "utf-8")

  encodeRiakObject :: Text -> ByteString
  encodeRiakObject =
    Text.encodeUtf8

  -- TODO text parse errors
  decodeRiakObject
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException Text
  decodeRiakObject content_type charset encoding bytes =
    case content_type of
      Just (ContentType "text/plain") ->
        (decode <=< decompress) bytes

      _ ->
        undefined

   where
    decompress :: ByteString -> Either SomeException ByteString
    decompress =
      case encoding of
        Nothing ->
          pure

        Just (ContentEncoding "ascii") ->
          pure

        Just (ContentEncoding "utf-8") ->
          pure

        x ->
          error (show x)

    decode :: ByteString -> Either SomeException Text
    decode =
      case charset of
        Nothing ->
          first toException . Text.decodeUtf8'

        Just (Charset "ascii") ->
          first toException . Text.decodeUtf8'

        Just (Charset "utf-8") ->
          first toException . Text.decodeUtf8'

        x ->
          error (show x)

instance IsRiakObject Aeson.Value where
  riakObjectContentType :: Aeson.Value -> Maybe ContentType
  riakObjectContentType _ =
    Just (ContentType "application/json")

  encodeRiakObject :: Aeson.Value -> ByteString
  encodeRiakObject =
    LazyByteString.toStrict . Aeson.encode

  decodeRiakObject
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException Aeson.Value
  decodeRiakObject content_type _ _ bytes =
    case content_type of
      Just (ContentType "application/json") ->
        case Aeson.eitherDecodeStrict' bytes of
          Left err ->
            error err

          Right value ->
            Right value

      x ->
        error (show x)


newtype JsonRiakObject a
  = JsonRiakObject { unJsonRiakObject :: a }
  deriving (Eq, Show)

instance (FromJSON a, ToJSON a) => IsRiakObject (JsonRiakObject a) where
  riakObjectContentType :: JsonRiakObject a -> Maybe ContentType
  riakObjectContentType _ =
    Just (ContentType "application/json")

  encodeRiakObject :: JsonRiakObject a -> ByteString
  encodeRiakObject =
    LazyByteString.toStrict . Aeson.encode . unJsonRiakObject

  decodeRiakObject
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException (JsonRiakObject a)
  decodeRiakObject content_type _ _ bytes =
    case content_type of
      Just (ContentType "application/json") ->
        case Aeson.eitherDecodeStrict' bytes of
          Left err ->
            error err

          Right value ->
            Right (JsonRiakObject value)

      x ->
        error (show x)


-- | Riak object charset.
newtype Charset
  = Charset { unCharset :: ByteString }
  deriving (Eq, Show)


-- | Riak object content encoding.
newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: ByteString }
  deriving (Eq, Show)
-- TODO ContentEncodingGzip


-- | Riak object content type.
newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Hashable)
