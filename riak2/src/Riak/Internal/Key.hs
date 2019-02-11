module Riak.Internal.Key where

import Riak.Internal.Prelude

import qualified Libriak.Proto      as Proto
import qualified Libriak.Proto.Lens as L

import Control.Lens  ((.~))
import Data.Hashable (Hashable)

import qualified ByteString


-- | A bucket type, bucket, and key.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Key
  = Key !ByteString !ByteString !ByteString
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

setProto ::
     ( Proto.HasLens' a "bucket" ByteString
     , Proto.HasLens' a "bucketType" ByteString
     , Proto.HasLens' a "key" ByteString
     )
  => Key
  -> a
  -> a
setProto (Key bucketType bucket key) proto =
  proto
    & L.bucket .~ bucket
    & L.bucketType .~ bucketType
    & L.key .~ key

setMaybeProto ::
     ( Proto.HasLens' a "bucket" ByteString
     , Proto.HasLens' a "bucketType" ByteString
     , Proto.HasLens' a "maybe'key" (Maybe ByteString)
     )
  => Key
  -> a
  -> a
setMaybeProto (Key bucketType bucket key) proto =
  proto
    & L.bucket .~ bucket
    & L.bucketType .~ bucketType
    & L.maybe'key .~
        (if ByteString.null key
          then Nothing
          else Just key)
