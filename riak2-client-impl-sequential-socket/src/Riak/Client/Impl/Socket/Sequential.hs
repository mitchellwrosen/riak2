module Riak.Client.Impl.Socket.Sequential
  ( Client
  , connect
  , disconnect
  , send
  , recv
  , exchange
  , stream
  ) where

import Riak.Message (Message)

import qualified Riak.Message as Message

import Control.Concurrent.MVar
import Data.ByteString         (ByteString)
import Data.IORef
import Network.Socket          (HostName, PortNumber, Socket)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)


data Client
  = Client
  { socket :: !Socket
  , bufferRef :: !(IORef ByteString)
  , lock :: !(MVar ())
  }

connect :: HostName -> PortNumber -> IO Client
connect host port = do
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
  lock <- newMVar ()

  pure Client
    { socket = socket
    , bufferRef = bufferRef
    , lock = lock
    }

disconnect :: Client -> IO ()
disconnect =
  Socket.close . socket

send :: Client -> Message -> IO ()
send client message =
  Socket.sendAll (socket client) (Message.encode message)

recv :: Client -> IO (Maybe Message)
recv client = do
  buffer <- readIORef (bufferRef client)

  loop
    (if ByteString.null buffer
      then Atto.Partial Message.parse
      else Message.parse buffer)

  where
    loop ::
         Atto.IResult ByteString Message
      -> IO (Maybe Message)
    loop = \case
      -- The message parser is just a 4-byte length followed by that many bytes,
      -- so just assume that can only fail due to not enough bytes.
      Atto.Fail _unconsumed _context _reason ->
        pure Nothing

      Atto.Partial k -> do
        bytes <- Socket.recv (socket client) 16384

        loop
          (if ByteString.null bytes
            then k ByteString.empty
            else Message.parse bytes)

      Atto.Done unconsumed message -> do
        writeIORef (bufferRef client) unconsumed
        pure (Just message)

exchange ::
     Client
  -> Message
  -> IO (Maybe Message)
exchange client request =
  withMVar (lock client) $ \_ -> do
    send client request
    recv client

stream ::
     Client
  -> Message
  -> (IO (Maybe Message) -> IO r)
  -> IO r
stream client request callback =
  withMVar (lock client) $ \_ -> do
    send client request
    callback (recv client)
