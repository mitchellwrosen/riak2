module Riak.Charset
  ( Charset(..)
  ) where

import Riak.Internal.Prelude

-- | Riak object charset.
newtype Charset
  = Charset { unCharset :: ByteString }
  deriving (Eq, Show)
