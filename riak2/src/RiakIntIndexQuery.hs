module RiakIntIndexQuery where

import RiakBucketInternal (Bucket)


-- | A query on an integer secondary index.
data IntIndexQuery
  = IntIndexQuery
  { bucket :: Bucket
  , index :: ByteString
  , minValue :: Int64
  , maxValue :: Int64
  } deriving stock (Generic, Show)
