module Riak.Internal.Key where

import Riak.Internal.Prelude

import Data.Hashable (Hashable)


-- | A bucket type, bucket, and key.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Key
  = Key !ByteString !ByteString !ByteString
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

