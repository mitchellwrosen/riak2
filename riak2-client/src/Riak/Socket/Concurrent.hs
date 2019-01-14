module Riak.Socket.Concurrent
  ( Socket
  , connect
  , close
  , exchange
  , stream
  , RecvResult(..)
  ) where

import Riak.Request           (Request)
import Riak.Response          (Response)
import Riak.Socket.Sequential (RecvResult(..))

import qualified Riak.Socket.Sequential as Sequential (Socket)
import qualified Riak.Socket.Sequential as Socket.Sequential

import Control.Concurrent.MVar
import Control.Foldl           (FoldM(..))
import Data.Coerce
import Network.Socket          (HostName, PortNumber)
import UnliftIO.Exception      (finally)


-- | A thread-safe connection to Riak.
data Socket
  = Socket
  { socket :: !Sequential.Socket
  , sync :: !Synchronized
  , relay :: !Relay
  }

-- | Connect to Riak.
connect :: HostName -> PortNumber -> IO Socket
connect host port =
  Socket
    <$> Socket.Sequential.connect host port
    <*> newSynchronized
    <*> newRelay

-- | Close the connection to Riak.
close :: Socket -> IO ()
close =
  Socket.Sequential.close . socket

exchange ::
     (Request a, Response b)
  => Socket
  -> a
  -> IO (RecvResult b)
exchange (Socket { socket, sync, relay}) request = do
  baton <-
    synchronized sync $ do
      Socket.Sequential.send socket request
      enterRelay relay

  withBaton baton (Socket.Sequential.recv socket)

stream ::
     (Request a, Response b)
  => Socket
  -> a -- ^ Request
  -> (b -> Bool) -- ^ Done?
  -> FoldM IO b r -- ^ Fold responses
  -> IO (RecvResult r)
stream (Socket { socket, sync, relay }) request done (FoldM step initial extract) = do
  baton <-
    synchronized sync $ do
      Socket.Sequential.send socket request
      enterRelay relay

  withBaton baton $ do
    let
      loop value =
        Socket.Sequential.recv socket >>= \case
          RiakClosedConnection ->
            pure RiakClosedConnection

          Failure err ->
            pure (Failure err)

          Success message -> do
            value' <-
              step value message

            if done message
              then
                Success <$> extract value'
              else
                loop value'

    loop =<< initial


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
