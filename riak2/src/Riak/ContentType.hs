module Riak.ContentType
  ( ContentType(..)
  ) where

import Riak.Internal.Prelude


-- | Riak object content type.
newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Hashable)
