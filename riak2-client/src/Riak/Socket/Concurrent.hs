module Riak.Socket.Concurrent
  ( Socket
  , connect
  , close
  , send
  , recv
  ) where

import Riak.Message  (Message)
import Riak.Proto    (RpbErrorResp)
import Riak.Request  (Request)
import Riak.Response (Response)

import qualified Riak.Response          as Response
import qualified Riak.Socket.Sequential as Sequential (Socket)
import qualified Riak.Socket.Sequential as Socket.Sequential

import Control.Concurrent.MVar
import Control.Monad.IO.Class    (MonadIO(liftIO))
import Network.Socket            (HostName, PortNumber)


-- | A thread-safe connection to Riak.
--
-- Sending an receiving may be performed concurrently via multiple threads, but
-- it is the user's responsibility to associate requests with their responses.
data Socket
  = Socket
  { socket :: !Sequential.Socket
  , sendlock :: !(MVar ())
  , recvlock :: !(MVar ())
  }

-- | Connect to Riak.
connect :: MonadIO m => HostName -> PortNumber -> m Socket
connect host port = liftIO $
  Socket
    <$> Socket.Sequential.connect host port
    <*> newMVar ()
    <*> newMVar ()

-- | Close the connection to Riak.
close :: MonadIO m => Socket -> m ()
close =
  Socket.Sequential.close . socket

-- | Send a request to Riak.
send :: (MonadIO m, Request a) => Socket -> a -> m ()
send (Socket { sendlock, socket }) request = liftIO $
  withMVar sendlock $ \() ->
    Socket.Sequential.send socket request

-- | Receive a response from Riak.
--
-- Returns an action that decodes the response received.
recv :: (MonadIO m, Response a) => Socket -> m (Either RpbErrorResp (m a))
recv (Socket { recvlock, socket }) = do
  message :: Message <-
    liftIO $ withMVar recvlock $ \() ->
      Socket.Sequential.recv socket

  Response.parse message

exchange ::
     (MonadIO m, Request a, Response b)
  => Socket
  -> a
  -> m (Either RpbErrorResp (m b))
exchange socket request =
  undefined
