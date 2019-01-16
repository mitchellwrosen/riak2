module Riak.IndexName
  ( IndexName(..)
  ) where

import Riak.Internal.Prelude

-- | A secondary index name.
newtype IndexName
  = IndexName { unIndexName :: ByteString }
  deriving (Eq, Show)
