module RiakBucketInternal where

import Data.Hashable (Hashable)


-- | A bucket type and bucket.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Bucket
  = Bucket !ByteString !ByteString
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)
