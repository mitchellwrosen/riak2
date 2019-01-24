module Riak.Interface.Impl.Socket
  ( Interface
  , EventHandlers(..)
  , new
  , connect
  , disconnect
  , send
  , receive
  , exchange
  , stream
  ) where

import Riak.Request  (Request)
import Riak.Response (DecodeError, Response)

import qualified Riak.Request  as Request
import qualified Riak.Response as Response

import Control.Concurrent.MVar
import Control.Exception       (throwIO)
import Data.ByteString         (ByteString)
import Data.IORef
import Network.Socket          (HostName, PortNumber, SockAddr, Socket)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Network.Socket                 as Network hiding (recv)
import qualified Network.Socket.ByteString      as Network (recv)
import qualified Network.Socket.ByteString.Lazy as Network (sendAll)


data Interface
  = Interface
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
  , onSend :: Request -> IO ()
  , onReceive :: Maybe Response -> IO ()
  }

new ::
     HostName
  -> PortNumber
  -> EventHandlers
  -> IO Interface
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

  pure Interface
    { sockaddr = Network.addrAddress info
    , socket = socket
    , bufferRef = bufferRef
    , lock = lock
    , handlers = handlers
    }

connect :: Interface -> IO ()
connect iface = do
  onConnect (handlers iface)
  Network.connect (socket iface) (sockaddr iface)

disconnect :: Interface -> IO ()
disconnect iface = do
  onDisconnect (handlers iface)
  Network.close (socket iface)
  writeIORef (bufferRef iface) ByteString.empty

send :: Interface -> Request -> IO ()
send iface request = do
  onSend (handlers iface) request
  Network.sendAll (socket iface) (Request.encode request)

receive :: Interface -> IO (Maybe Response)
receive iface = do
  buffer <- readIORef (bufferRef iface)

  result :: Maybe Response <-
    loop
      (if ByteString.null buffer
        then Atto.Partial Response.parse
        else Response.parse buffer)

  onReceive (handlers iface) result
  pure result

  where
    loop ::
         Atto.IResult ByteString (Either DecodeError Response)
      -> IO (Maybe Response)
    loop = \case
      -- The response parser is just a 4-byte length followed by that many
      -- bytes, so just assume that can only fail due to not enough bytes.
      Atto.Fail _unconsumed _context _reason ->
        pure Nothing

      Atto.Partial k -> do
        bytes <- Network.recv (socket iface) 16384

        loop
          (if ByteString.null bytes
            then k ByteString.empty
            else Response.parse bytes)

      Atto.Done unconsumed result ->
        case result of
          Left err ->
            throwIO err

          Right response -> do
            writeIORef (bufferRef iface) unconsumed
            pure (Just response)

exchange ::
     Interface
  -> Request
  -> IO (Maybe Response)
exchange iface request =
  withMVar (lock iface) $ \_ -> do
    send iface request
    receive iface

stream ::
     Interface
  -> Request
  -> (IO (Maybe Response) -> IO r)
  -> IO r
stream iface request callback =
  withMVar (lock iface) $ \_ -> do
    send iface request
    callback (receive iface)
