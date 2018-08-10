{-# LANGUAGE ScopedTypeVariables #-}

module Riak
  ( Handle
  , withHandle
  ) where

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Network.Socket          (AddrInfo(..), HostName, PortNumber, Socket,
                                SocketType(Stream), defaultHints, getAddrInfo)
import UnliftIO.Exception      (bracket)

import qualified Network.Socket as Network

data Handle
  = Handle !Socket

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Handle -> m a)
  -> m a
withHandle host port =
  bracket (connect host port) disconnect

connect :: MonadIO m => HostName -> PortNumber -> m Handle
connect host port = liftIO $ do
  info : _ <-
    let
      hints =
        defaultHints { addrSocketType = Stream }
    in
      getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Network.socket (addrFamily info) (addrSocketType info) (addrProtocol info)

  pure (Handle socket)

disconnect :: MonadIO m => Handle -> m ()
disconnect (Handle socket) =
  liftIO (Network.close socket)
