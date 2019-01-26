module Riak.Interface.Impl.Socket
  ( Interface
  , EventHandlers(..)
  , new
  , connect
  , disconnect
  , exchange
  , stream
  ) where

import Riak.Request  (Request)
import Riak.Response (DecodeError, Response)
import Riak.Socket   (Socket)

import qualified Riak.Request  as Request
import qualified Riak.Response as Response
import qualified Riak.Socket   as Socket

import Control.Concurrent.MVar
import Control.Exception       (throwIO)
import Data.ByteString         (ByteString)
import Data.IORef

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as ByteString


data Interface
  = Interface
  { socket :: !Socket
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
     Socket -- ^
  -> EventHandlers -- ^
  -> IO Interface
new socket handlers = do
  lock <- newMVar ()

  pure Interface
    { socket = socket
    , lock = lock
    , handlers = handlers
    }

connect ::
     Interface -- ^
  -> IO ()
connect iface = do
  onConnect (handlers iface)
  Socket.connect (socket iface)

disconnect ::
     Interface -- ^
  -> IO ()
disconnect iface = do
  onDisconnect (handlers iface)
  Socket.disconnect (socket iface)

send ::
     Interface
  -> Request
  -> IO ()
send iface request = do
  onSend (handlers iface) request
  Socket.send (socket iface) request

receive ::
     Interface -- ^
  -> IO (Maybe Response)
receive iface = do
  response :: Maybe Response <-
    Socket.receive (socket iface)
  onReceive (handlers iface) response
  pure response

exchange ::
     Interface -- ^
  -> Request -- ^
  -> IO (Maybe Response)
exchange iface request =
  withMVar (lock iface) $ \_ -> do
    send iface request
    receive iface

stream ::
     Interface -- ^
  -> Request -- ^
  -> (IO (Maybe Response) -> IO r) -- ^
  -> IO r
stream iface request callback =
  withMVar (lock iface) $ \_ -> do
    send iface request
    callback (receive iface)
