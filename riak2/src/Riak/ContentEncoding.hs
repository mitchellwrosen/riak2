module Riak.ContentEncoding
  ( ContentEncoding(..)
  ) where

import Riak.Internal.Prelude

-- | Riak object content encoding.
newtype ContentEncoding
  = ContentEncoding { unContentEncoding :: ByteString }
  deriving (Eq, Show)
-- TODO ContentEncodingGzip
