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
import Riak.Response (Response)
import Riak.Socket   (Socket)

import qualified Riak.Socket   as Socket

import Control.Concurrent.MVar


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

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 <> EventHandlers a2 b2 c2 d2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

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
