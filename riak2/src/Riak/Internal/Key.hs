module Riak.Internal.Key where

import Riak.Internal.Prelude

import qualified Libriak.Proto as Proto

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
     , Proto.HasLens' a "key" ByteString
     , Proto.HasLens' a "type'" ByteString
     )
  => Key
  -> a
  -> a
setProto (Key bucketType bucket key) proto =
  proto
    & Proto.bucket .~ bucket
    & Proto.key .~ key
    & Proto.type' .~ bucketType

setMaybeProto ::
     ( Proto.HasLens' a "bucket" ByteString
     , Proto.HasLens' a "maybe'key" (Maybe ByteString)
     , Proto.HasLens' a "type'" ByteString
     )
  => Key
  -> a
  -> a
setMaybeProto (Key bucketType bucket key) proto =
  proto
    & Proto.bucket .~ bucket
    & Proto.maybe'key .~
        (if ByteString.null key
          then Nothing
          else Just key)
    & Proto.type' .~ bucketType
