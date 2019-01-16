module Riak.Bucket
  ( Bucket(..)
  , pattern DefaultBucket
  ) where

import Riak.BucketType       (BucketType, pattern DefaultBucketType)
import Riak.Internal.Prelude

import Data.Text.Encoding (decodeUtf8')

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.Text              as Text


-- | A bucket type and bucket.
data Bucket
  = Bucket !BucketType !ByteString
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show Bucket where
  show :: Bucket -> String
  show (Bucket type' bucket) =
    show type'
    ++ " " ++
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode bucket)))
      Text.unpack
      (decodeUtf8' bucket)

-- | (De)construct a bucket in the the @default@ bucket type. Its usage is
-- discouraged.
pattern DefaultBucket :: ByteString -> Bucket
pattern DefaultBucket name =
  Bucket DefaultBucketType name
