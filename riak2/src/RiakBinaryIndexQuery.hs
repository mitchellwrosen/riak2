module RiakBinaryIndexQuery where

import RiakBucketInternal (Bucket(..))


-- | A query on a binary secondary index.
data BinaryIndexQuery
  = BinaryIndexQuery
  { bucket :: Bucket
  , index :: ByteString
  , minValue :: ByteString
  , maxValue :: ByteString
  } deriving stock (Generic, Show)

-- | Build a query on the built-in index @\"\$bucket\"@, which indexes each
-- object by its bucket.
inBucket ::
     Bucket -- ^
  -> BinaryIndexQuery
inBucket bucket@(Bucket _ b) =
  BinaryIndexQuery
    { bucket = bucket
    , index = builtinBucketIndex
    , minValue = b
    , maxValue = b
    }

-- | Build a query on the built-in index @\"\$key\"@, which indexes each object
-- by its key.
keysBetween ::
     Bucket -- ^
  -> ByteString -- ^ Min key segment
  -> ByteString -- ^ Max key segment
  -> BinaryIndexQuery
keysBetween bucket minValue maxValue =
  BinaryIndexQuery
    { index = builtinKeyIndex
    , ..
    }

builtinBucketIndex :: ByteString
builtinBucketIndex =
  "$bucket"

builtinKeyIndex :: ByteString
builtinKeyIndex =
  "$key"

indexName :: BinaryIndexQuery -> ByteString
indexName BinaryIndexQuery { index } =
  if index == builtinBucketIndex || index == builtinKeyIndex
    then index
    else index <> "_bin"
