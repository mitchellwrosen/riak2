module Riak.Client.Impl.Socket.Sequential
  ( Client
  , EventHandlers(..)
  , new
  , connect
  , disconnect
  , send
  , receive
  , exchange
  , stream
  ) where

import Riak.Message (Message)

import qualified Riak.Message as Message

import Control.Concurrent.MVar
import Data.ByteString         (ByteString)
import Data.IORef
import Network.Socket          (HostName, PortNumber, SockAddr, Socket)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Network.Socket                 as Network hiding (recv)
import qualified Network.Socket.ByteString      as Network (recv)
import qualified Network.Socket.ByteString.Lazy as Network (sendAll)


data Client
  = Client
  { sockaddr :: !SockAddr
  , socket :: !Socket
  , bufferRef :: !(IORef ByteString)
  , lock :: !(MVar ())
  , handlers :: !EventHandlers
  }

data EventHandlers
  = EventHandlers
  { onConnect :: IO ()
  , onDisconnect :: IO ()
  , onSend :: Message -> IO ()
  , onReceive :: Maybe Message -> IO ()
  }

new ::
     HostName
  -> PortNumber
  -> EventHandlers
  -> IO Client
new host port handlers = do
  info : _ <-
    let
      hints =
        Network.defaultHints { Network.addrSocketType = Network.Stream }
    in
      Network.getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Network.socket
      (Network.addrFamily info)
      (Network.addrSocketType info)
      (Network.addrProtocol info)

  bufferRef <- newIORef ByteString.empty
  lock <- newMVar ()

  pure Client
    { sockaddr = Network.addrAddress info
    , socket = socket
    , bufferRef = bufferRef
    , lock = lock
    , handlers = handlers
    }

connect :: Client -> IO ()
connect client = do
  onConnect (handlers client)
  Network.connect (socket client) (sockaddr client)

disconnect :: Client -> IO ()
disconnect client = do
  onDisconnect (handlers client)
  Network.close (socket client)
  writeIORef (bufferRef client) ByteString.empty

send :: Client -> Message -> IO ()
send client message = do
  onSend (handlers client) message
  Network.sendAll (socket client) (Message.encode message)

receive :: Client -> IO (Maybe Message)
receive client = do
  buffer <- readIORef (bufferRef client)

  result :: Maybe Message <-
    loop
      (if ByteString.null buffer
        then Atto.Partial Message.parse
        else Message.parse buffer)

  onReceive (handlers client) result
  pure result

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
        bytes <- Network.recv (socket client) 16384

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
    receive client

stream ::
     Client
  -> Message
  -> (IO (Maybe Message) -> IO r)
  -> IO r
stream client request callback =
  withMVar (lock client) $ \_ -> do
    send client request
    callback (receive client)
