module Riak.Bucket
  ( Bucket(..)
  ) where

import Riak.Internal.Prelude


-- | A bucket type and bucket.
data Bucket
  = Bucket
  { type' :: !ByteString
  , bucket :: !ByteString
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)
