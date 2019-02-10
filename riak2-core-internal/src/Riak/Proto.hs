module Riak.Proto
  ( DecodeError(..)
  , module Proto.Proto.Riak
    -- ** Re-exports
  , defMessage
  , HasLens'
  ) where

import Control.Exception      (Exception)
import Data.ByteString        (ByteString)
import Data.ProtoLens.Message (defMessage)
import Data.Word              (Word8)
import Lens.Labels            (HasLens')
import Proto.Proto.Riak

data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnknownMessageCode !Word8 !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)
