module Riak.BucketType
  ( BucketType(..)
  ) where

import Riak.Internal.Prelude


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
newtype BucketType
  = BucketType
  { type' :: ByteString
  } deriving stock (Eq, Show)
    deriving newtype (Hashable)
