module Riak.Key
  ( Key(..)
  ) where

import Riak.Internal.Prelude


-- | A bucket type, bucket, and key
data Key
  = Key
  { type' :: !ByteString
  , bucket :: !ByteString
  , key :: !ByteString
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)
