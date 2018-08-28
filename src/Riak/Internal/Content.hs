{-# LANGUAGE DataKinds, DeriveFunctor, DerivingStrategies, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving, InstanceSigs,
             LambdaCase, MagicHash, MultiParamTypeClasses, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, TypeFamilies,
             UndecidableInstances #-}

module Riak.Internal.Content
  ( Charset(..)
  , RiakContent(..)
  , ContentEncoding(..)
  , ContentType(..)
  , IsRiakContent(..)
  ) where

import Data.Bifunctor         (first)
import Data.Time
import Lens.Family2.Unchecked (lens)
import Lens.Labels

import qualified Data.Text.Encoding as Text

import Riak.Internal.Prelude
import Riak.Internal.Types


data RiakContent a
  = RiakContent
      !(RiakLocation 'Nothing)
      a                       -- Value
      (Maybe ContentType)     -- Content type
      (Maybe Charset)         -- Charset
      (Maybe ContentEncoding) -- Content encoding
      (Maybe RiakVtag)        -- Vtag
      (Maybe UTCTime)         -- Last modified
      RiakMetadata            -- User metadata
      [RiakSecondaryIndex]    -- Secondary indexes
      Bool                    -- Deleted
      TTL                     -- TTL
  deriving (Eq, Functor, Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (RiakContent s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (RiakContent s) (RiakContent t) x a b where
  lensOf = lensOf'

-- TODO content lenses for location, namespace, type, bucket, key

instance Functor f => HasLens' f (RiakContent a)                 "location"        (RiakLocation 'Nothing)   where lensOf' _ = lens (\(RiakContent x _ _ _ _ _ _ _ _ _ _) -> x) (\(RiakContent _ b c d e f g h i j k) x -> RiakContent x b c d e f g h i j k)
instance Functor f => HasLens  f (RiakContent a) (RiakContent b) "value"           a                       b where lensOf  _ = lens (\(RiakContent _ x _ _ _ _ _ _ _ _ _) -> x) (\(RiakContent a _ c d e f g h i j k) x -> RiakContent a x c d e f g h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "contentType"     (Maybe ContentType)       where lensOf' _ = lens (\(RiakContent _ _ x _ _ _ _ _ _ _ _) -> x) (\(RiakContent a b _ d e f g h i j k) x -> RiakContent a b x d e f g h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "charset"         (Maybe Charset)           where lensOf' _ = lens (\(RiakContent _ _ _ x _ _ _ _ _ _ _) -> x) (\(RiakContent a b c _ e f g h i j k) x -> RiakContent a b c x e f g h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "contentEncoding" (Maybe ContentEncoding)   where lensOf' _ = lens (\(RiakContent _ _ _ _ x _ _ _ _ _ _) -> x) (\(RiakContent a b c d _ f g h i j k) x -> RiakContent a b c d x f g h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "vtag"            (Maybe RiakVtag)          where lensOf' _ = lens (\(RiakContent _ _ _ _ _ x _ _ _ _ _) -> x) (\(RiakContent a b c d e _ g h i j k) x -> RiakContent a b c d e x g h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "lastMod"         (Maybe UTCTime)           where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ x _ _ _ _) -> x) (\(RiakContent a b c d e f _ h i j k) x -> RiakContent a b c d e f x h i j k)
instance Functor f => HasLens' f (RiakContent a)                 "usermeta"        RiakMetadata              where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ _ x _ _ _) -> x) (\(RiakContent a b c d e f g _ i j k) x -> RiakContent a b c d e f g x i j k)
instance Functor f => HasLens' f (RiakContent a)                 "indexes"         [RiakSecondaryIndex]      where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ _ _ x _ _) -> x) (\(RiakContent a b c d e f g h _ j k) x -> RiakContent a b c d e f g h x j k)
instance Functor f => HasLens' f (RiakContent a)                 "deleted"         Bool                      where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ _ _ _ x _) -> x) (\(RiakContent a b c d e f g h i _ k) x -> RiakContent a b c d e f g h i x k)
instance Functor f => HasLens' f (RiakContent a)                 "ttl"             TTL                       where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ _ _ _ _ x) -> x) (\(RiakContent a b c d e f g h i j _) x -> RiakContent a b c d e f g h i j x)


-- TODO JsonRiakContent

-- | 'IsRiakContent' classifies types that are stored in Riak objects. Every
-- object must have a content type, and may optionally have a character set and
-- encoding.
--
-- For convenience, two instances are provided by this library, to store binary
-- and textual data.
--
-- [__@ByteString@__]
--
-- * When reading,
--
--     * Content type is ignored.
--
--     * Charset is ignored.
--
--     * Content encoding is ignored.
--
--     This allows you to read any Riak object as a raw 'ByteString',
--     regardless of its type.
--
-- * When writing,
--
--     * Content type is set to @application/octet-stream@.
--
--     * Charset is empty.
--
--     * Content encoding is empty.
--
-- [__@Text@__]
--
-- * When reading,
--
--     * Content type must be @text/plain@.
--
--     * Charset must be empty, @ascii@, or @utf-8@.
--
--     * Content encoding must be empty, @ascii@, or @utf-8@.
--
-- * When writing,
--
--     * Content type is set to @text/plain@.
--
--     * Charset is set to @utf-8@.
--
--     * Content encoding is set to @utf-8@.
--
-- When writing your own 'IsRiakContent' instances, use any content type,
-- charset, and content encoding you wish. None of them are actually required by
-- Riak, they are for you to help make sense of your own data.
class IsRiakContent a where
  -- | The content type.
  riakContentType :: a -> ContentType

  -- | The character set of the content. Defaults to 'Nothing'.
  riakCharset :: a -> Maybe Charset
  riakCharset _ =
    Nothing

  -- | The encoding of the content. Defaults to 'Nothing'.
  riakContentEncoding :: a -> Maybe ContentEncoding
  riakContentEncoding _ =
    Nothing

  -- | Encode a Riak object.
  encodeRiakContent :: a -> ByteString

  -- | Decode a Riak object, given its content type, charset, encoding, and
  -- encoded value.
  decodeRiakContent
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException a

instance IsRiakContent ByteString where
  riakContentType :: ByteString -> ContentType
  riakContentType _ =
    ContentType "application/octet-stream"

  encodeRiakContent :: ByteString -> ByteString
  encodeRiakContent =
    id

  decodeRiakContent
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException ByteString
  decodeRiakContent _ _ _ =
    Right

instance IsRiakContent Text where
  riakContentType :: Text -> ContentType
  riakContentType _ =
    ContentType "text/plain"

  riakCharset :: Text -> Maybe Charset
  riakCharset _ =
    Just (Charset "utf-8")

  riakContentEncoding :: Text -> Maybe ContentEncoding
  riakContentEncoding _ =
    Just (ContentEncoding "utf-8")

  encodeRiakContent :: Text -> ByteString
  encodeRiakContent =
    Text.encodeUtf8

  -- TODO text parse errors
  decodeRiakContent
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException Text
  decodeRiakContent type' charset encoding =
    case type' of
      Just (ContentType "text/plain") ->
        decode <=< decompress

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
  deriving newtype (IsString)
