module Libriak.Proto
  ( DecodeError(..)
    -- ** Re-exports
  , module Data.Riak.Proto
  , defMessage
  , HasLens'
  ) where

import Control.Exception      (Exception)
import Data.ByteString        (ByteString)
import Data.ProtoLens.Message (defMessage)
import Data.Word              (Word8)
import Lens.Labels            (HasLens')
import Data.Riak.Proto

data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnknownMessageCode !Word8 !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)
