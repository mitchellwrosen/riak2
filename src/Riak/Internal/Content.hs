{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, InstanceSigs, LambdaCase, MagicHash,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings,
             PatternSynonyms, TypeFamilies, UndecidableInstances #-}

module Riak.Internal.Content
  ( Charset(..)
  , RiakContent(..)
  , ContentEncoding(..)
  , pattern ContentEncodingNone
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


-- TODO add back content type, charset, content encoding
data RiakContent a
  = RiakContent
      !(RiakLocation 'Nothing)
      a                    -- Value
      (Maybe RiakVtag)     -- Vtag
      (Maybe UTCTime)      -- Last modified
      RiakMetadata         -- User metadata
      [RiakSecondaryIndex] -- Secondary indexes
      Bool                 -- Deleted
      TTL                  -- TTL
  deriving (Eq, Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (RiakContent s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (RiakContent s) (RiakContent t) x a b where
  lensOf = lensOf'

-- TODO content lenses for location, namespace, type, bucket, key

instance Functor f => HasLens' f (RiakContent a)                 "location" (RiakLocation 'Nothing)   where lensOf' _ = lens (\(RiakContent x _ _ _ _ _ _ _) -> x) (\(RiakContent _ b c d e f g h) x -> RiakContent x b c d e f g h)
instance Functor f => HasLens  f (RiakContent a) (RiakContent b) "value"    a                       b where lensOf  _ = lens (\(RiakContent _ x _ _ _ _ _ _) -> x) (\(RiakContent a _ c d e f g h) x -> RiakContent a x c d e f g h)
instance Functor f => HasLens' f (RiakContent a)                 "vtag"     (Maybe RiakVtag)          where lensOf' _ = lens (\(RiakContent _ _ x _ _ _ _ _) -> x) (\(RiakContent a b _ d e f g h) x -> RiakContent a b x d e f g h)
instance Functor f => HasLens' f (RiakContent a)                 "lastMod"  (Maybe UTCTime)           where lensOf' _ = lens (\(RiakContent _ _ _ x _ _ _ _) -> x) (\(RiakContent a b c _ e f g h) x -> RiakContent a b c x e f g h)
instance Functor f => HasLens' f (RiakContent a)                 "usermeta" RiakMetadata              where lensOf' _ = lens (\(RiakContent _ _ _ _ x _ _ _) -> x) (\(RiakContent a b c d _ f g h) x -> RiakContent a b c d x f g h)
instance Functor f => HasLens' f (RiakContent a)                 "indexes"  [RiakSecondaryIndex]      where lensOf' _ = lens (\(RiakContent _ _ _ _ _ x _ _) -> x) (\(RiakContent a b c d e _ g h) x -> RiakContent a b c d e x g h)
instance Functor f => HasLens' f (RiakContent a)                 "deleted"  Bool                      where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ x _) -> x) (\(RiakContent a b c d e f _ h) x -> RiakContent a b c d e f x h)
instance Functor f => HasLens' f (RiakContent a)                 "ttl"      TTL                       where lensOf' _ = lens (\(RiakContent _ _ _ _ _ _ _ x) -> x) (\(RiakContent a b c d e f g _) x -> RiakContent a b c d e f g x)


class IsRiakContent a where
  riakContentType :: a -> ContentType

  riakCharset :: a -> Charset

  riakContentEncoding :: a -> ContentEncoding

  encodeRiakContent :: a -> ByteString

  decodeRiakContent
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
    -> ByteString
    -> Either SomeException a

instance IsRiakContent ByteString where
  riakContentType :: ByteString -> ContentType
  riakContentType _ =
    ContentTypeApplicationOctetStream

  riakCharset :: ByteString -> Charset
  riakCharset _ =
    CharsetNone

  riakContentEncoding :: ByteString -> ContentEncoding
  riakContentEncoding _ =
    ContentEncodingNone

  encodeRiakContent :: ByteString -> ByteString
  encodeRiakContent =
    id

  decodeRiakContent
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
    -> ByteString
    -> Either SomeException ByteString
  decodeRiakContent _ _ _ =
    Right

instance IsRiakContent Text where
  riakContentType :: Text -> ContentType
  riakContentType _ =
    ContentTypeTextPlain

  riakCharset :: Text -> Charset
  riakCharset _ =
    CharsetNone

  riakContentEncoding :: Text -> ContentEncoding
  riakContentEncoding _ =
    ContentEncodingNone

  encodeRiakContent :: Text -> ByteString
  encodeRiakContent =
    Text.encodeUtf8

  -- TODO text parse errors
  decodeRiakContent
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
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
        ContentEncodingNone ->
          pure

        ContentEncodingAscii ->
          pure

        ContentEncodingUtf8 ->
          pure

        _ ->
          undefined

    decode :: ByteString -> Either SomeException Text
    decode =
      case charset of
        CharsetNone ->
          first toException . Text.decodeUtf8'

        CharsetAscii ->
          first toException . Text.decodeUtf8'

        CharsetUtf8 ->
          first toException . Text.decodeUtf8'

        _ ->
          undefined


newtype Charset
  = Charset { unCharset :: Maybe ByteString }

pattern CharsetNone :: Charset
pattern CharsetNone =
  Charset Nothing

pattern CharsetAscii :: Charset
pattern CharsetAscii =
  Charset Ascii

pattern CharsetUtf8 :: Charset
pattern CharsetUtf8 =
  Charset Utf8


-- TODO ContentEncodingGzip
newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: Maybe ByteString }

pattern ContentEncodingNone :: ContentEncoding
pattern ContentEncodingNone =
  ContentEncoding Nothing

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


pattern Ascii :: Maybe ByteString
pattern Ascii =
  Just "ascii"

pattern Utf8 :: Maybe ByteString
pattern Utf8 =
  Just "utf-8"
