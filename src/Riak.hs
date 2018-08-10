{-# LANGUAGE ScopedTypeVariables #-}

module Riak
  ( Handle
  , withHandle
  ) where

import Control.Monad
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.Void
import Data.Word
import Network.Socket          (AddrInfo(..), HostName, PortNumber, Socket,
                                SocketType(Stream), defaultHints, getAddrInfo)
import UnliftIO.Exception      (bracket)

import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Builder   as Builder
import qualified Data.ByteString.Lazy      as Lazy (ByteString)
import qualified Data.ByteString.Streaming as Q
import qualified Network.Socket            as Network hiding (recv, sendAll)
import qualified Network.Socket.ByteString as Network (recv)
import qualified Network.Socket.ByteString.Lazy as Network (sendAll)

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Socket                         -- Server socket
      !(IORef (Q.ByteString IO Void)) -- Infinite input from server
      (Lazy.ByteString -> IO ())

data Message
  = Message
      !Word8      -- Message code
      !ByteString -- Message payload

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

  let
    sink :: Lazy.ByteString -> IO ()
    sink =
      Network.sendAll socket

  pure (Handle socket sourceRef sink)

disconnect :: MonadIO m => Handle -> m ()
disconnect (Handle socket _ _) =
  liftIO (Network.close socket)

-- | Send a 'Message' on a 'Handle'.
send :: Handle -> Message -> IO ()
send (Handle _ _ sink) (Message code bytes) =
  sink payload
 where
  payload :: Lazy.ByteString
  payload =
    Builder.toLazyByteString
      (Builder.int32BE (fromIntegral (ByteString.length bytes + 1))
        <> Builder.word8 code
        <> Builder.byteString bytes)
