module Riak.Socket.Sequential
  ( Socket
  , connect
  , close
  , send
  , recv
  , SocketError(..)
  ) where

import Riak.Message (Message)
import Riak.Request (Request)

import qualified Riak.Message as Message
import qualified Riak.Request as Request

import Control.Exception      (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString        (ByteString)
import Data.IORef
import Network.Socket         (HostName, PortNumber)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)


data SocketError
  = EOF
  | ParseFailure ![String] String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | A non-thread-safe connection to Riak.
data Socket
  = Socket
  { socket    :: !Socket.Socket
  , bufferRef :: !(IORef ByteString)
  }

-- | Connect to Riak.
connect :: MonadIO m => HostName -> PortNumber -> m Socket
connect host port = liftIO $ do
  info : _ <-
    let
      hints =
        Socket.defaultHints { Socket.addrSocketType = Socket.Stream }
    in
      Socket.getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket.Socket <-
    Socket.socket
      (Socket.addrFamily info)
      (Socket.addrSocketType info)
      (Socket.addrProtocol info)

  Socket.connect socket (Socket.addrAddress info)

  bufferRef <- newIORef ByteString.empty

  pure Socket
    { socket = socket
    , bufferRef = bufferRef
    }

-- | Close the connection to Riak.
close :: MonadIO m => Socket -> m ()
close =
  liftIO . Socket.close . socket

-- | Send a request to Riak.
send :: (MonadIO m, Request a) => Socket -> a -> m ()
send (Socket { socket }) request =
  liftIO (Socket.sendAll socket (Message.encode (Request.toMessage request)))

-- | Receive a response from Riak.
recv :: MonadIO m => Socket -> m Message
recv (Socket { bufferRef, socket }) = liftIO $ do
  buffer <- readIORef bufferRef

  loop
    (if ByteString.null buffer
      then Atto.Partial Message.parse
      else Message.parse buffer)

  where
    loop :: Atto.IResult ByteString Message -> IO Message
    loop = \case
      Atto.Fail _unconsumed context reason ->
        throwIO (ParseFailure context reason)

      Atto.Partial k -> do
        bytes <- Socket.recv socket 16384

        loop
          (if ByteString.null bytes
            then k ByteString.empty
            else Message.parse bytes)

      Atto.Done unconsumed message -> do
        writeIORef bufferRef unconsumed
        pure message
