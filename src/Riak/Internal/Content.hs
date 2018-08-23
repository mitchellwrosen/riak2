{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, InstanceSigs, LambdaCase, MagicHash,
             MultiParamTypeClasses, OverloadedStrings, PatternSynonyms,
             TypeFamilies, UndecidableInstances #-}

module Riak.Internal.Content
  ( Charset(..)
  , Content(..)
  , ContentEncoding(..)
  , pattern ContentEncodingNone
  , ContentType(..)
  , pattern ContentTypeApplicationOctetStream
  , pattern ContentTypeTextPlain
  , IsContent(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Bifunctor         (first)
import Data.ByteString        (ByteString)
import Data.Text              (Text)
import Data.Time
import GHC.Exts               (IsString)
import Lens.Family2.Unchecked (lens)
import Lens.Labels
import Prelude                hiding ((.))

import qualified Data.Text.Encoding as Text

import Riak.Internal.Types


-- TODO add back content type, charset, content encoding
data Content a
  = Content
      !(Location 'Nothing)
      a                -- Value
      (Maybe Vtag)     -- Vtag
      (Maybe UTCTime)  -- Last modified
      Metadata         -- User metadata
      [SecondaryIndex] -- Secondary indexes
      Bool             -- Deleted
      TTL              -- TTL
  deriving (Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (Content s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (Content s) (Content t) x a b where
  lensOf = lensOf'

-- TODO content lenses for location, namespace, type, bucket, key

instance Functor f => HasLens' f (Content a)             "location" (Location 'Nothing)     where lensOf' _ = lens (\(Content x _ _ _ _ _ _ _) -> x) (\(Content _ b c d e f g h) x -> Content x b c d e f g h)
instance Functor f => HasLens  f (Content a) (Content b) "value"    a                     b where lensOf  _ = lens (\(Content _ x _ _ _ _ _ _) -> x) (\(Content a _ c d e f g h) x -> Content a x c d e f g h)
instance Functor f => HasLens' f (Content a)             "vtag"     (Maybe Vtag)            where lensOf' _ = lens (\(Content _ _ x _ _ _ _ _) -> x) (\(Content a b _ d e f g h) x -> Content a b x d e f g h)
instance Functor f => HasLens' f (Content a)             "lastMod"  (Maybe UTCTime)         where lensOf' _ = lens (\(Content _ _ _ x _ _ _ _) -> x) (\(Content a b c _ e f g h) x -> Content a b c x e f g h)
instance Functor f => HasLens' f (Content a)             "usermeta" Metadata                where lensOf' _ = lens (\(Content _ _ _ _ x _ _ _) -> x) (\(Content a b c d _ f g h) x -> Content a b c d x f g h)
instance Functor f => HasLens' f (Content a)             "indexes"  [SecondaryIndex]        where lensOf' _ = lens (\(Content _ _ _ _ _ x _ _) -> x) (\(Content a b c d e _ g h) x -> Content a b c d e x g h)
instance Functor f => HasLens' f (Content a)             "deleted"  Bool                    where lensOf' _ = lens (\(Content _ _ _ _ _ _ x _) -> x) (\(Content a b c d e f _ h) x -> Content a b c d e f x h)
instance Functor f => HasLens' f (Content a)             "ttl"      TTL                     where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ x) -> x) (\(Content a b c d e f g _) x -> Content a b c d e f g x)


class IsContent a where
  contentEncode :: a -> (ContentType, Charset, ContentEncoding, ByteString)

  contentDecode
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
    -> ByteString
    -> Either SomeException a

instance IsContent ByteString where
  contentEncode
    :: ByteString
    -> (ContentType, Charset, ContentEncoding, ByteString)
  contentEncode bytes =
    ( ContentTypeApplicationOctetStream
    , CharsetNone
    , ContentEncodingNone
    , bytes
    )

  contentDecode
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
    -> ByteString
    -> Either SomeException ByteString
  contentDecode _ _ _ =
    Right

instance IsContent Text where
  contentEncode :: Text -> (ContentType, Charset, ContentEncoding, ByteString)
  contentEncode bytes =
    ( ContentTypeTextPlain
    , CharsetNone
    , ContentEncodingNone
    , Text.encodeUtf8 bytes
    )

  -- TODO text parse errors
  contentDecode
    :: Maybe ContentType
    -> Charset
    -> ContentEncoding
    -> ByteString
    -> Either SomeException Text
  contentDecode type' charset encoding =
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
