module Riak.Socket.Concurrent
  ( Socket
  , connect
  , close
  , exchange
  , RecvError(..)
  ) where

import Riak.Proto             (RpbErrorResp)
import Riak.Request           (Request)
import Riak.Response          (Response)
import Riak.Socket.Sequential (RecvError(..))

import qualified Riak.Socket.Sequential as Sequential (Socket)
import qualified Riak.Socket.Sequential as Socket.Sequential

import Control.Concurrent.MVar
import Data.Coerce
import Network.Socket          (HostName, PortNumber)
import UnliftIO.Exception      (finally)


-- | A thread-safe connection to Riak.
newtype Socket
  = Socket (ReqRep Sequential.Socket)

-- | Connect to Riak.
connect :: HostName -> PortNumber -> IO Socket
connect host port =
  coerce (newReqRep =<< Socket.Sequential.connect host port)

-- | Close the connection to Riak.
close :: Socket -> IO ()
close =
  Socket.Sequential.close . resource . coerce

exchange ::
     (Request a, Response b)
  => Socket
  -> a
  -> IO (Either RecvError (Either RpbErrorResp b))
exchange (Socket socket) request =
  reqRep
    socket
    (\socket -> Socket.Sequential.send socket request)
    Socket.Sequential.recv


data ReqRep a
  = ReqRep
  { resource :: a
  , sync :: !Synchronized
  , relay :: !Relay
  }

newReqRep :: a -> IO (ReqRep a)
newReqRep resource =
  ReqRep
    <$> pure resource
    <*> newSynchronized
    <*> newRelay

reqRep ::
     ReqRep a
  -> (a -> IO ())
  -> (a -> IO b)
  -> IO b
reqRep (ReqRep { resource, sync, relay }) send recv = do
  baton <-
    synchronized sync $ do
      send resource
      enterRelay relay

  withBaton baton (recv resource)


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
