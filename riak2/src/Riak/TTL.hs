module Riak.TTL
  ( TTL(..)
  ) where

import Riak.Internal.Prelude


newtype TTL
  = TTL { unTTL :: Maybe Word32 }
  deriving stock (Eq, Show)
