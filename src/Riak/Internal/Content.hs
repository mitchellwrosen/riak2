{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, InstanceSigs, LambdaCase, MagicHash,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings,
             PatternSynonyms, TypeFamilies, UndecidableInstances #-}

module Riak.Internal.Content
  ( Charset(..)
  , RiakContent(..)
  , ContentEncoding(..)
  , ContentType(..)
  , pattern ContentTypeApplicationOctetStream
  , pattern ContentTypeTextPlain
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
  deriving (Eq, Show)

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


-- | 'IsRiakContent' classifies types that are stored in Riak objects. Every
-- object must have a content type, and may optionally have a character set and
-- encoding.
class IsRiakContent a where
  riakContentType :: a -> ContentType

  -- | The character set of the content. Defaults to 'Nothing'.
  riakCharset :: a -> Maybe Charset
  riakCharset _ =
    Nothing

  -- | The encoding of the content. Defaults to 'Nothing'.
  riakContentEncoding :: a -> Maybe ContentEncoding
  riakContentEncoding _ =
    Nothing

  encodeRiakContent :: a -> ByteString

  decodeRiakContent
    :: Maybe ContentType
    -> Maybe Charset
    -> Maybe ContentEncoding
    -> ByteString
    -> Either SomeException a

instance IsRiakContent ByteString where
  riakContentType :: ByteString -> ContentType
  riakContentType _ =
    ContentTypeApplicationOctetStream

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
    ContentTypeTextPlain

  riakCharset :: Text -> Maybe Charset
  riakCharset _ =
    Just CharsetUtf8

  riakContentEncoding :: Text -> Maybe ContentEncoding
  riakContentEncoding _ =
    Just ContentEncodingUtf8

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
      Just ContentTypeTextPlain ->
        decode <=< decompress

      _ ->
        undefined

   where
    decompress :: ByteString -> Either SomeException ByteString
    decompress =
      case encoding of
        Nothing ->
          pure

        Just ContentEncodingAscii ->
          pure

        Just ContentEncodingUtf8 ->
          pure

        _ ->
          undefined

    decode :: ByteString -> Either SomeException Text
    decode =
      case charset of
        Nothing ->
          first toException . Text.decodeUtf8'

        Just CharsetAscii ->
          first toException . Text.decodeUtf8'

        Just CharsetUtf8 ->
          first toException . Text.decodeUtf8'

        _ ->
          undefined


newtype Charset
  = Charset { unCharset :: ByteString }
  deriving (Eq, Show)

pattern CharsetAscii :: Charset
pattern CharsetAscii =
  Charset Ascii

pattern CharsetUtf8 :: Charset
pattern CharsetUtf8 =
  Charset Utf8


-- TODO ContentEncodingGzip
newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: ByteString }
  deriving (Eq, Show)

pattern ContentEncodingAscii :: ContentEncoding
pattern ContentEncodingAscii =
  ContentEncoding Ascii

pattern ContentEncodingUtf8 :: ContentEncoding
pattern ContentEncodingUtf8 =
  ContentEncoding Utf8


newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

pattern ContentTypeApplicationOctetStream :: ContentType
pattern ContentTypeApplicationOctetStream =
  ContentType "application/octet-stream"

pattern ContentTypeTextPlain :: ContentType
pattern ContentTypeTextPlain =
  ContentType "text/plain"


pattern Ascii :: ByteString
pattern Ascii =
  "ascii"

pattern Utf8 :: ByteString
pattern Utf8 =
  "utf-8"
