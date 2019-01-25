module Socket.Impl.Network
  ( Socket
  , new
  , connect
  , disconnect
  , send
  , receive
    -- * Re-exports
  , Network.HostName
  , Network.PortNumber
  ) where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Lazy           as Lazy (ByteString)
import qualified Network.Socket                 as Network hiding (recv)
import qualified Network.Socket.ByteString      as Network (recv)
import qualified Network.Socket.ByteString.Lazy as Network (sendAll)

data Socket
  = Socket Network.Socket Network.SockAddr

new ::
     Network.HostName -- ^
  -> Network.PortNumber -- ^
  -> IO Socket
new host port = do
  info : _ <-
    let
      hints =
        Network.defaultHints { Network.addrSocketType = Network.Stream }
    in
      Network.getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Network.Socket <-
    Network.socket
      (Network.addrFamily info)
      (Network.addrSocketType info)
      (Network.addrProtocol info)

  pure (Socket socket (Network.addrAddress info))

connect :: Socket -> IO ()
connect (Socket socket addr) =
  Network.connect socket addr

disconnect :: Socket -> IO ()
disconnect (Socket socket _) =
  Network.close socket

receive :: Socket -> Int -> IO ByteString
receive (Socket socket _) n =
  Network.recv socket 16384

send :: Socket -> Lazy.ByteString -> IO ()
send (Socket socket _) bytes =
  Network.sendAll socket bytes
