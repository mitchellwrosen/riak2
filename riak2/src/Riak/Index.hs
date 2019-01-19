module Riak.Index
  ( Index(..)
  ) where

import Riak.Internal.Prelude

-- TODO RiakIndex values should be a set
-- | A secondary index.
data Index
  = IndexInt !ByteString !Int64
  | IndexBin !ByteString !ByteString
  deriving (Eq, Show)
