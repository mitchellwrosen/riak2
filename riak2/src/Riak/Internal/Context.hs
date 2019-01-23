module Riak.Internal.Context where

import Riak.Internal.Prelude

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base64 as Base64


-- | The opaque causal context attached to an object or data type.
newtype Context
  = Context { unContext :: ByteString }
  deriving stock (Eq)

instance Show Context where
  show :: Context -> String
  show =
    coerce (show . Base64.encode)

-- | The "none" context. Use this when updating an object or data type for the
-- first time.
none :: Context
none =
  Context ByteString.empty

