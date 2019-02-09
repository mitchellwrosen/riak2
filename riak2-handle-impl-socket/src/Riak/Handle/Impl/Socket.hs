module Riak.Handle.Impl.Socket
  ( Handle
  , Config(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
  , Exception(..)
  , isRemoteShutdownException
    -- ** Re-exports
  , Socket.Socket(..)
  , Socket.new1
  ) where

import Riak.Request  (Request)
import Riak.Response (Response)
import Riak.Socket (Socket)

import qualified Riak.Socket as Socket

import Control.Concurrent.MVar
import UnliftIO.Exception      (bracket_, throwIO)

import qualified Control.Exception as Exception


data Handle
  = Handle
  { socket :: !Socket
  , lock :: !(MVar ())
  , handlers :: !EventHandlers
  }

data Config
  = Config
  { socket :: !Socket
  , handlers :: !EventHandlers
  }

data EventHandlers
  = EventHandlers
  { onSend :: Request -> IO ()
  , onReceive :: Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle Config { socket, handlers } k = do
  lock <- newMVar ()

  bracket_
    (Socket.connect socket)
    (Socket.disconnect socket)
    (k Handle
      { socket = socket
      , lock = lock
      , handlers = handlers
      })

send ::
     Handle
  -> Request
  -> IO ()
send Handle { socket, handlers } request = do
  onSend handlers request
  Socket.send socket request

receive ::
     Handle -- ^
  -> IO Response
receive Handle { socket, handlers } =
  Socket.receive socket >>= \case
    Nothing ->
      throwIO RemoteShutdown

    Just response -> do
      onReceive handlers response
      pure response

-- | Send a request and receive the response (a single message).
--
-- /Throws/. If Riak closes the connection, throws 'RemoteShutdown'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO Response
exchange iface request =
  withMVar (lock iface) $ \_ -> do
    send iface request
    receive iface

-- | Send a request and stream the response (one or more messages).
--
-- /Throws/. If Riak closes the connection, throws 'RemoteShutdown'.
stream ::
     Handle -- ^
  -> Request -- ^
  -> (IO Response -> IO r) -- ^
  -> IO r
stream iface request callback =
  withMVar (lock iface) $ \_ -> do
    send iface request
    callback (receive iface)


data Exception
  = RemoteShutdown
  deriving stock (Show)
  deriving anyclass (Exception.Exception)

isRemoteShutdownException :: Exception -> Bool
isRemoteShutdownException _ =
  True
