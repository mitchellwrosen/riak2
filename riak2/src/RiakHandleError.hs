module RiakHandleError where

import Libriak.Connection (ConnectionError)
import Libriak.Response   (DecodeError)


data HandleError :: Type where
  HandleConnectionError :: ConnectionError -> HandleError
  HandleDecodeError :: DecodeError -> HandleError
  -- | A request timed out.
  HandleTimeoutError :: HandleError
  deriving stock (Eq, Show)
