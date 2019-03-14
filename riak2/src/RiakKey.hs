module RiakKey
  ( Key(..)
  , keyBucketType
  , keyBucket
  , keyBucketSegment
  , keyKeySegment
  , generatedKey

  , isGeneratedKey
  , fromProto
  , setProto
  , setMaybeProto
  ) where

import RiakBucketInternal     (Bucket(..))
import RiakBucketTypeInternal (BucketType)
import RiakKeyInternal        (Key(..))

import qualified RiakBucketType as BucketType

import Control.Lens                       (Lens', (.~), (^.))
import Data.Hashable                      (Hashable)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')

import qualified Data.ByteString as ByteString
import qualified Data.Riak.Proto as Proto


-- | A lens onto the bucket type of a key.
--
-- @
-- Key bucketType bucket key
--     `————————´
-- @
keyBucketType :: Lens' Key BucketType
keyBucketType f (Key bucketType bucket key) =
  (\bucketType -> Key bucketType bucket key) <$>
    f bucketType

-- | A lens onto the bucket of a key.
--
-- @
-- Key bucketType bucket key
--     `———————————————´
-- @
keyBucket :: Lens' Key Bucket
keyBucket f (Key bucketType bucket key) =
  (\(Bucket bucketType bucket) -> Key bucketType bucket key) <$>
    f (Bucket bucketType bucket)

-- | A lens onto the bucket segment of a key.
--
-- @
-- Key bucketType bucket key
--                `————´
-- @
keyBucketSegment :: Lens' Key ByteString
keyBucketSegment f (Key bucketType bucket key) =
  (\bucket -> Key bucketType bucket key) <$>
    f bucket

-- | A lens onto the key segment of a key.
--
-- @
-- Key bucketType bucket key
--                       `—´
-- @
keyKeySegment :: Lens' Key ByteString
keyKeySegment f (Key bucketType bucket key) =
  (\key -> Key bucketType bucket key) <$>
    f key

-- | Use 'generatedKey' to ask Riak to generate a random key when writing a new
-- object or data type.
generatedKey ::
     Bucket -- ^
  -> Key
generatedKey (Bucket bucketType bucket) =
  Key bucketType bucket ByteString.empty

isGeneratedKey :: Key -> Bool
isGeneratedKey (Key _ _ key) =
  ByteString.null key

fromProto ::
     ( HasLens' a "bucket" ByteString
     , HasLens' a "key" ByteString
     , HasLens' a "type'" ByteString
     )
  => a
  -> Key
fromProto proto =
  Key
    (BucketType.fromProto proto)
    (proto ^. Proto.bucket)
    (proto ^. Proto.key)

setProto ::
     ( HasLens' a "bucket" ByteString
     , HasLens' a "key" ByteString
     , HasLens' a "type'" ByteString
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
     ( HasLens' a "bucket" ByteString
     , HasLens' a "maybe'key" (Maybe ByteString)
     , HasLens' a "type'" ByteString
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
