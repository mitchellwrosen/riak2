module Riak.Interface.Impl.Socket.Concurrent
  ( Interface
  , EventHandlers(..)
  , new
  , connect
  , disconnect
  , exchange
  , stream
  ) where

import Control.Concurrent.MVar
import Data.Coerce             (coerce)
import Riak.Request            (Request)
import Riak.Response           (Response)
import Riak.Socket             (Socket)
import UnliftIO.Exception      (finally)

import qualified Riak.Socket as Socket


data Interface
  = Interface
  { socket :: !Socket
  , sync :: !Synchronized
  , relay :: !Relay
  }

data EventHandlers
  = EventHandlers

new ::
     Socket
  -> EventHandlers
  -> IO Interface
new socket _ =
  Interface
    <$> pure socket
    <*> newSynchronized
    <*> newRelay

connect :: Interface -> IO ()
connect iface =
  Socket.connect (socket iface)

disconnect :: Interface -> IO ()
disconnect =
  Socket.disconnect . socket

exchange ::
     Interface
  -> Request
  -> IO (Maybe Response)
exchange iface request = do
  baton :: Baton <-
    synchronized (sync iface) $ do
      Socket.send (socket iface) request
      enterRelay (relay iface)

  withBaton baton
    (Socket.receive (socket iface))

stream ::
     Interface
  -> Request
  -> (IO (Maybe Response) -> IO r)
  -> IO r
stream iface request callback =
  -- Riak request handling state machine is odd. Streaming responses are
  -- special; when one is active, no other requests can be serviced on this
  -- socket. I learned this the hard way by reading Riak source code.
  --
  -- So, hold a lock for the entirety of the request-response exchange, not just
  -- during sending the request.
  synchronized (sync iface) $ do
    Socket.send (socket iface) request
    callback (Socket.receive (socket iface))


newtype Synchronized
  = Synchronized (MVar ())

newSynchronized :: IO Synchronized
newSynchronized =
  coerce (newMVar ())

synchronized :: Synchronized -> IO a -> IO a
synchronized (Synchronized var) =
  withMVar var . const


newtype Relay
  = Relay (MVar (MVar ()))

data Baton
  = Baton (MVar ()) (MVar ())

newRelay :: IO Relay
newRelay =
  coerce (newMVar =<< newMVar ())

enterRelay :: Relay -> IO Baton
enterRelay (Relay var) = do
  after <- newEmptyMVar
  before <- swapMVar var after
  pure (Baton before after)

-- TODO think about async exceptions a bit here
withBaton :: Baton -> IO a -> IO a
withBaton (Baton before after) action = do
  takeMVar before
  action `finally` putMVar after ()
