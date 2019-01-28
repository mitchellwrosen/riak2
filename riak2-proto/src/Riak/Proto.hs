module Riak.Proto
  ( DecodeError(..)
  , module Proto.Proto.Riak
  , defMessage
  ) where

import Control.Exception      (Exception)
import Data.ByteString        (ByteString)
import Data.ProtoLens.Message (defMessage)
import Data.Word              (Word8)
import Proto.Proto.Riak

data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnknownMessageCode !Word8 !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)
