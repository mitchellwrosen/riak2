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
import Lens.Family2.Unchecked (lens)
import Lens.Labels

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding   as Text

import Riak.Internal.Prelude
import Riak.Internal.Types

data RiakObject a
  = RiakObject
      !(RiakKey 'Nothing)
      a                       -- Value
      (Maybe ContentType)     -- Content type
      (Maybe Charset)         -- Charset
      (Maybe ContentEncoding) -- Content encoding
      (Maybe RiakVtag)        -- Vtag
      (Maybe UTCTime)         -- Last modified
      RiakMetadata            -- User metadata
      [RiakIndex]             -- Secondary indexes
      Bool                    -- Deleted
      TTL                     -- TTL
  deriving (Eq, Functor, Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (RiakObject s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (RiakObject s) (RiakObject t) x a b where
  lensOf = lensOf'

instance Functor f => HasLens' f (RiakObject a)                "key"             (RiakKey 'Nothing)        where lensOf' _ = lens (\(RiakObject x _ _ _ _ _ _ _ _ _ _) -> x) (\(RiakObject _ b c d e f g h i j k) x -> RiakObject x b c d e f g h i j k)
instance Functor f => HasLens  f (RiakObject a) (RiakObject b) "value"           a                       b where lensOf  _ = lens (\(RiakObject _ x _ _ _ _ _ _ _ _ _) -> x) (\(RiakObject a _ c d e f g h i j k) x -> RiakObject a x c d e f g h i j k)
instance Functor f => HasLens' f (RiakObject a)                "contentType"     (Maybe ContentType)       where lensOf' _ = lens (\(RiakObject _ _ x _ _ _ _ _ _ _ _) -> x) (\(RiakObject a b _ d e f g h i j k) x -> RiakObject a b x d e f g h i j k)
instance Functor f => HasLens' f (RiakObject a)                "charset"         (Maybe Charset)           where lensOf' _ = lens (\(RiakObject _ _ _ x _ _ _ _ _ _ _) -> x) (\(RiakObject a b c _ e f g h i j k) x -> RiakObject a b c x e f g h i j k)
instance Functor f => HasLens' f (RiakObject a)                "contentEncoding" (Maybe ContentEncoding)   where lensOf' _ = lens (\(RiakObject _ _ _ _ x _ _ _ _ _ _) -> x) (\(RiakObject a b c d _ f g h i j k) x -> RiakObject a b c d x f g h i j k)
instance Functor f => HasLens' f (RiakObject a)                "vtag"            (Maybe RiakVtag)          where lensOf' _ = lens (\(RiakObject _ _ _ _ _ x _ _ _ _ _) -> x) (\(RiakObject a b c d e _ g h i j k) x -> RiakObject a b c d e x g h i j k)
instance Functor f => HasLens' f (RiakObject a)                "lastMod"         (Maybe UTCTime)           where lensOf' _ = lens (\(RiakObject _ _ _ _ _ _ x _ _ _ _) -> x) (\(RiakObject a b c d e f _ h i j k) x -> RiakObject a b c d e f x h i j k)
instance Functor f => HasLens' f (RiakObject a)                "usermeta"        RiakMetadata              where lensOf' _ = lens (\(RiakObject _ _ _ _ _ _ _ x _ _ _) -> x) (\(RiakObject a b c d e f g _ i j k) x -> RiakObject a b c d e f g x i j k)
instance Functor f => HasLens' f (RiakObject a)                "indexes"         [RiakIndex]               where lensOf' _ = lens (\(RiakObject _ _ _ _ _ _ _ _ x _ _) -> x) (\(RiakObject a b c d e f g h _ j k) x -> RiakObject a b c d e f g h x j k)
instance Functor f => HasLens' f (RiakObject a)                "deleted"         Bool                      where lensOf' _ = lens (\(RiakObject _ _ _ _ _ _ _ _ _ x _) -> x) (\(RiakObject a b c d e f g h i _ k) x -> RiakObject a b c d e f g h i x k)
instance Functor f => HasLens' f (RiakObject a)                "ttl"             TTL                       where lensOf' _ = lens (\(RiakObject _ _ _ _ _ _ _ _ _ _ x) -> x) (\(RiakObject a b c d e f g h i j _) x -> RiakObject a b c d e f g h i j x)


-- | 'IsRiakObject' classifies types that are stored in Riak objects. Every
-- object /should/ have a content type, /may/ have a character set and /may/
-- have an encoding.
--
-- For convenience, instances are provided by this library, to store binary,
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


newtype Charset
  = Charset { unCharset :: ByteString }
  deriving (Eq, Show)


-- TODO ContentEncodingGzip
newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: ByteString }
  deriving (Eq, Show)


newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Hashable)
