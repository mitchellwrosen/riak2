module Riak.Index
  ( Index(..)
  ) where

import Riak.IndexName
import Riak.Internal.Prelude

-- TODO RiakIndex values should be a set
-- | A secondary index.
data Index
  = IndexInt !IndexName !Int64
  | IndexBin !IndexName !ByteString
  deriving (Eq, Show)
