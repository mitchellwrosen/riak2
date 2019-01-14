module Riak.Socket.Sequential
  ( Socket
  , connect
  , close
  , send
  , recv
  , RecvError(..)
  ) where

import Riak.Message  (Message)
import Riak.Proto    (RpbErrorResp)
import Riak.Request  (Request)
import Riak.Response (Response)

import qualified Riak.Message  as Message
import qualified Riak.Request  as Request
import qualified Riak.Response as Response

import Data.Bifunctor  (first)
import Data.ByteString (ByteString)
import Data.IORef
import Network.Socket  (HostName, PortNumber)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)


-- | A non-thread-safe connection to Riak.
data Socket
  = Socket
  { socket :: !Socket.Socket
  , bufferRef :: !(IORef ByteString)
  }

-- | Connect to Riak.
connect :: HostName -> PortNumber -> IO Socket
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

  pure Socket
    { socket = socket
    , bufferRef = bufferRef
    }

-- | Close the connection to Riak.
close :: Socket -> IO ()
close =
  Socket.close . socket

-- | Send a request to Riak.
send :: Request a => Socket -> a -> IO ()
send (Socket { socket }) request =
  Socket.sendAll socket (Message.encode (Request.toMessage request))

data RecvError
  = EOF
  | ParseError Response.ParseError

-- | Receive a response from Riak.
recv ::
     forall a.
     Response a
  => Socket
  -> IO (Either RecvError (Either RpbErrorResp a))
recv (Socket { bufferRef, socket }) = do
  buffer <- readIORef bufferRef

  loop
    (if ByteString.null buffer
      then Atto.Partial Message.parse
      else Message.parse buffer)

  where
    loop ::
         Atto.IResult ByteString Message
      -> IO (Either RecvError (Either RpbErrorResp a))
    loop = \case
      -- The message parser is just a 4-byte length followed by that many bytes,
      -- so just assume that can only fail due to not enough bytes.
      Atto.Fail _unconsumed _context _reason ->
        pure (Left EOF)

      Atto.Partial k -> do
        bytes <- Socket.recv socket 16384

        loop
          (if ByteString.null bytes
            then k ByteString.empty
            else Message.parse bytes)

      Atto.Done unconsumed message -> do
        writeIORef bufferRef unconsumed

        pure (first ParseError (Response.parse message))
