module Riak.Handle
  ( Handle
  , HandleConfig(..)
  , HandleError(..)
  , DecodeError(..)
  , DisconnectReason(..)
  , EventHandlers(..)
  , createHandle
  ) where

import Libriak.Response (DecodeError(..))
import RiakHandle
import RiakHandleError  (HandleError(..))
