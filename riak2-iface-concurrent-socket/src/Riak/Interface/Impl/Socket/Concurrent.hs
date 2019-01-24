module Riak.Interface.Impl.Socket.Concurrent
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

import Riak.Interface.Impl.Socket (EventHandlers(..))

import qualified Riak.Interface.Impl.Socket as Inner

import Control.Concurrent.MVar
import Data.Coerce             (coerce)
import Network.Socket          (HostName, PortNumber, SockAddr)
import UnliftIO.Exception      (finally)


data Interface
  = Interface
  { inner :: !Inner.Interface
  , sync :: !Synchronized
  , relay :: !Relay
  }

new ::
     HostName
  -> PortNumber
  -> EventHandlers
  -> IO Interface
new host port handlers =
  Interface
    <$> Inner.new host port handlers
    <*> newSynchronized
    <*> newRelay

connect :: Interface -> IO ()
connect iface =
  Inner.connect (inner iface)

disconnect :: Interface -> IO ()
disconnect =
  Inner.disconnect . inner

exchange ::
     Interface
  -> Request
  -> IO (Maybe Response)
exchange iface request = do
  baton :: Baton <-
    synchronized (sync iface) $ do
      Inner.send (inner iface) request
      enterRelay (relay iface)

  withBaton baton
    (Inner.receive (inner iface))

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
    Inner.send (inner iface) request
    callback (Inner.receive (inner iface))


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
