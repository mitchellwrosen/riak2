module Riak.Key
  ( Key(..)
  , none
  ) where

import Riak.Internal.Prelude

import qualified Data.ByteString as ByteString


-- | A bucket type, bucket, and key.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Key
  = Key
  { type' :: !ByteString
  , bucket :: !ByteString
  , key :: !ByteString
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- Use 'none' to ask Riak to generate a random key when writing a new object or
-- data type:
--
-- @
-- Key
--   { type' = ...
--   , bucket = ...
--   , key = Riak.Key.none
--   }
-- @
none :: ByteString
none =
  ByteString.empty
