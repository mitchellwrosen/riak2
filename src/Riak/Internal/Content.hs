{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, InstanceSigs, LambdaCase, MagicHash,
             MultiParamTypeClasses, OverloadedStrings, PatternSynonyms,
             TypeFamilies, UndecidableInstances #-}

module Riak.Internal.Content
  ( Content(..)
  , ContentEncoding(..)
  , pattern ContentEncodingNone
  , ContentType(..)
  , IsContent(..)
  ) where

import Control.Exception
import Data.Bifunctor         (first)
import Data.ByteString        (ByteString)
import Data.Text              (Text)
import Data.Time
import Data.Word
import GHC.Exts               (IsString)
import Lens.Family2.Unchecked (lens)
import Lens.Labels
import Prelude                hiding ((.))

import qualified Data.Text.Encoding as Text

import Riak.Internal.Types

data Content a
  = Content
      a                                -- Value
      (Maybe ByteString)               -- Charset
      (Maybe Vtag)
      (Maybe UTCTime)                  -- Last modified
      [(ByteString, Maybe ByteString)] -- User meta
      [(ByteString, Maybe ByteString)] -- Indexes
      Bool                             -- Deleted
      (Maybe Word32)                   -- TTL
  deriving (Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (Content s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (Content s) (Content t) x a b where
  lensOf = lensOf'

instance Functor f => HasLens  f (Content a) (Content b) "value"           a                                b where lensOf  _ = lens (\(Content x _ _ _ _ _ _ _) -> x) (\(Content _ b c d e f g h) x -> Content x b c d e f g h)
instance Functor f => HasLens' f (Content a)             "charset"         (Maybe ByteString)                 where lensOf' _ = lens (\(Content _ x _ _ _ _ _ _) -> x) (\(Content a _ c d e f g h) x -> Content a x c d e f g h)
instance Functor f => HasLens' f (Content a)             "vtag"            (Maybe Vtag)                       where lensOf' _ = lens (\(Content _ _ x _ _ _ _ _) -> x) (\(Content a b _ d e f g h) x -> Content a b x d e f g h)
instance Functor f => HasLens' f (Content a)             "lastMod"         (Maybe UTCTime)                    where lensOf' _ = lens (\(Content _ _ _ x _ _ _ _) -> x) (\(Content a b c _ e f g h) x -> Content a b c x e f g h)
instance Functor f => HasLens' f (Content a)             "usermeta"        [(ByteString, Maybe ByteString)]   where lensOf' _ = lens (\(Content _ _ _ _ x _ _ _) -> x) (\(Content a b c d _ f g h) x -> Content a b c d x f g h)
instance Functor f => HasLens' f (Content a)             "indexes"         [(ByteString, Maybe ByteString)]   where lensOf' _ = lens (\(Content _ _ _ _ _ x _ _) -> x) (\(Content a b c d e _ g h) x -> Content a b c d e x g h)
instance Functor f => HasLens' f (Content a)             "deleted"         Bool                               where lensOf' _ = lens (\(Content _ _ _ _ _ _ x _) -> x) (\(Content a b c d e f _ h) x -> Content a b c d e f x h)
instance Functor f => HasLens' f (Content a)             "ttl"             (Maybe Word32)                     where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ x) -> x) (\(Content a b c d e f g _) x -> Content a b c d e f g x)


class IsContent a where
  contentType     :: Proxy# a -> ContentType
  contentEncoding :: a -> ContentEncoding
  contentEncode   :: a -> ByteString
  contentDecode   :: ByteString -> Either SomeException a

instance IsContent ByteString where
  contentType :: Proxy# ByteString -> ContentType
  contentType _ =
    "application/octet-stream"

  contentEncoding :: ByteString -> ContentEncoding
  contentEncoding _ =
    ContentEncodingNone

  contentEncode :: ByteString -> ByteString
  contentEncode =
    id

  contentDecode :: ByteString -> Either SomeException ByteString
  contentDecode =
    Right

instance IsContent Text where
  contentType :: Proxy# Text -> ContentType
  contentType _ =
    "text/plain"

  contentEncoding :: Text -> ContentEncoding
  contentEncoding _ =
    ContentEncodingNone

  contentEncode :: Text -> ByteString
  contentEncode =
    Text.encodeUtf8

  contentDecode :: ByteString -> Either SomeException Text
  contentDecode =
    first toException . Text.decodeUtf8'


newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: Maybe ByteString }

pattern ContentEncodingNone :: ContentEncoding
pattern ContentEncodingNone = ContentEncoding Nothing

-- TODO more content encodings

newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)
