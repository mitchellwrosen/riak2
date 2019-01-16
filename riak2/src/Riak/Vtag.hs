module Riak.Vtag
  ( Vtag(..)
  ) where

import Riak.Internal.Prelude


newtype Vtag
  = Vtag { unVtag :: ByteString }
  deriving (Eq, Show)
