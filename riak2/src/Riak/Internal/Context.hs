module Riak.Internal.Context where

import Riak.Internal.Prelude

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base64 as Base64


newtype Context
  = Context { unContext :: ByteString }
  deriving stock (Eq)

instance Show Context where
  show :: Context -> String
  show =
    coerce (show . Base64.encode)

none :: Context
none =
  Context ByteString.empty

