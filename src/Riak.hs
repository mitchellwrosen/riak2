{-# LANGUAGE ScopedTypeVariables #-}

module Riak
  ( Handle
  , withHandle
  ) where

import Control.Monad
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Data.ByteString         (ByteString)
import Data.IORef
import Data.Void
import Network.Socket          (AddrInfo(..), HostName, PortNumber, Socket,
                                SocketType(Stream), defaultHints, getAddrInfo)
import UnliftIO.Exception      (bracket)

import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Streaming as Q
import qualified Network.Socket            as Network hiding (recv)
import qualified Network.Socket.ByteString as Network (recv)

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Socket                         -- Server socket
      !(IORef (Q.ByteString IO Void)) -- Infinite input from server

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

  let
    source :: Q.ByteString IO Void
    source =
      forever $ do
        bytes :: ByteString <-
          liftIO (Network.recv socket 4096)
        if ByteString.null bytes
          -- TODO Properly handle Riak closing connection
          then error "Riak closed the connection"
          else Q.chunk bytes

  sourceRef :: IORef (Q.ByteString IO Void) <-
    newIORef source

  pure (Handle socket sourceRef)

disconnect :: MonadIO m => Handle -> m ()
disconnect (Handle socket _) =
  liftIO (Network.close socket)
