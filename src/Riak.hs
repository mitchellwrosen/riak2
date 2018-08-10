module Riak
  ( Handle
  , withHandle
  ) where

import Control.Monad.IO.Unlift
import Network.Socket          (HostName, PortNumber)

import Riak.Internal.Connection

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle !Connection

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Handle -> m a)
  -> m a
withHandle host port f =
  withConnection host port (f . Handle)
