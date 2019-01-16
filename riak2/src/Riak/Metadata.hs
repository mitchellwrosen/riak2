module Riak.Metadata
  ( Metadata(..)
  ) where

import Riak.Internal.Prelude

-- | Arbitrary metadata.
newtype Metadata
  = Metadata { unMetadata :: [(ByteString, Maybe ByteString)] }
  deriving (Eq, Show)
