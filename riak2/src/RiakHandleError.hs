module RiakHandleError where

import Libriak.Connection (ConnectionError(..))
import Libriak.Response   (DecodeError)


-- TODO flatten DecodeError
data HandleError :: Type where
  HandleDecodeError :: DecodeError -> HandleError
  HandleLocalShutdownError :: HandleError
  HandleRemoteResetError :: HandleError
  HandleRemoteShutdownError :: HandleError
  HandleTimeoutError :: HandleError
  deriving stock (Eq, Show)

fromConnectionError :: ConnectionError -> HandleError
fromConnectionError = \case
  LocalShutdown -> HandleLocalShutdownError
  RemoteReset -> HandleRemoteResetError
  RemoteShutdown -> HandleRemoteShutdownError
  RemoteTimeout -> HandleTimeoutError
