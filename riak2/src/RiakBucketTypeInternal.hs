module RiakBucketTypeInternal
  ( BucketType
  , defaultBucketType
  ) where


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
type BucketType
  = ByteString

-- | The default bucket type.
defaultBucketType :: BucketType
defaultBucketType =
  "default"
