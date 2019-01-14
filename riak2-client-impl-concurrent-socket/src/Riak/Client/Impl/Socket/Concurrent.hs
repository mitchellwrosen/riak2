module Riak.Client.Impl.Socket.Concurrent
  ( Client
  , connect
  , disconnect
  , exchange
  , stream
  ) where

import Riak.Message (Message)

import qualified Riak.Client.Impl.Socket.Sequential as Sequential (Client)
import qualified Riak.Client.Impl.Socket.Sequential as Client.Sequential

import Control.Concurrent.MVar
import Data.Coerce             (coerce)
import Network.Socket          (HostName, PortNumber)
import UnliftIO.Exception      (finally)


data Client
  = Client
  { socket :: !Sequential.Client
  , sync :: !Synchronized
  , relay :: !Relay
  }

connect :: HostName -> PortNumber -> IO Client
connect host port =
  Client
    <$> Client.Sequential.connect host port
    <*> newSynchronized
    <*> newRelay

disconnect :: Client -> IO ()
disconnect =
  Client.Sequential.disconnect . socket

exchange ::
     Client
  -> Message
  -> IO (Maybe Message)
exchange client request = do
  baton <-
    synchronized (sync client) $ do
      Client.Sequential.send (socket client) request
      enterRelay (relay client)

  withBaton baton
    (Client.Sequential.recv (socket client))

stream ::
     Client
  -> Message
  -> (IO (Maybe Message) -> IO r)
  -> IO r
stream client request callback =
  -- Riak request handling state machine is odd. Streaming responses are
  -- special; when one is active, no other requests can be serviced on this
  -- socket. I learned this the hard way by reading Riak source code.
  --
  -- So, hold a lock for the entirety of the request-response exchange, not just
  -- during sending the request.
  synchronized (sync client) $ do
    Client.Sequential.send (socket client) request
    callback (Client.Sequential.recv (socket client))


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
