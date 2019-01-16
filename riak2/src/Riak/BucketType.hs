module Riak.BucketType
  ( BucketType(..)
  , pattern DefaultBucketType
  ) where

import Riak.Internal.Prelude

import Data.Text.Encoding (decodeUtf8)

import qualified Data.Text as Text


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
newtype BucketType
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show BucketType where
  show :: BucketType -> String
  show =
    Text.unpack . decodeUtf8 . unBucketType

-- | (De)construct the @default@ bucket type. Its usage is discouraged.
pattern DefaultBucketType :: BucketType
pattern DefaultBucketType =
  BucketType "default"
