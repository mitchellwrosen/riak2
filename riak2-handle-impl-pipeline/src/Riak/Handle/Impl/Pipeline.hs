module Riak.Handle.Impl.Pipeline
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
    -- ** Re-exports
  , ConnectError(..)
  , ConnectionError(..)
  , Endpoint(..)
  ) where

import Libriak.Connection (ConnectError(..), Connection, ConnectionError(..),
                           Endpoint(..), withConnection)
import Libriak.Request    (Request)
import Libriak.Response   (Response)

import qualified Libriak.Connection as Connection

import Control.Concurrent.MVar
import Data.Coerce             (coerce)


data Handle
  = Handle
  { connection :: !Connection
  , sync :: !Synchronized
  , relay :: !Relay
  , handlers :: !EventHandlers
  }

data HandleConfig
  = HandleConfig
  { endpoint :: !Endpoint
  , handlers :: !EventHandlers
  }

data EventHandlers
  = EventHandlers
  { onSend :: Request -> IO ()
  , onReceive :: Either ConnectionError Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)


-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either ConnectError a)
withHandle HandleConfig { endpoint, handlers } onSuccess = do
  sync :: Synchronized <-
    newSynchronized

  relay :: Relay <-
    newRelay

  withConnection endpoint $ \connection ->
    onSuccess Handle
      { connection = connection
      , sync = sync
      , relay = relay
      , handlers = handlers
      }

-- | Send a request.
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
send ::
     Handle
  -> Request
  -> IO (Either ConnectionError ())
send Handle { connection, handlers } request = do
  onSend handlers request
  Connection.send connection request

-- | Receive a response.
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
receive ::
     Handle -- ^
  -> IO (Either ConnectionError Response)
receive Handle { connection, handlers } = do
  response <- Connection.receive connection
  onReceive handlers response
  pure response


-- | Send a request and receive the response (a single message).
--
-- /Throws/: If another prior thread crashed while using this socket, throws
-- 'Control.Exception.BlockedIndefinitelyOnMVar'.
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO (Either ConnectionError Response)
exchange handle@(Handle { sync, relay }) request = do
  synchronized sync doSend >>= \case
    Left err ->
      pure (Left err)

    Right baton ->
      withBaton baton (receive handle)

  where
    doSend :: IO (Either ConnectionError Baton)
    doSend =
      send handle request >>= \case
        Left err ->
          pure (Left err)

        Right () ->
          Right <$> enterRelay relay


-- | Send a request and stream the response (one or more messages).
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either ConnectionError r)
stream handle@(Handle { sync }) request value0 step =
  -- Riak request handling state machine is odd. Streaming responses are
  -- special; when one is active, no other requests can be serviced on this
  -- socket. I learned this the hard way by reading Riak source code.
  --
  -- So, hold a lock for the entirety of the request-response exchange, not just
  -- during sending the request.
  synchronized sync $ do
    send handle request >>= \case
      Left err ->
        pure (Left err)

      Right () ->
        consume value0

  where
    consume :: x -> IO (Either ConnectionError r)
    consume value =
      receive handle >>= \case
        Left err ->
          pure (Left err)

        Right response ->
          step value response >>= \case
            Left newValue ->
              consume newValue
            Right result ->
              pure (Right result)


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

withBaton :: Baton -> IO a -> IO a
withBaton (Baton before after) action = do
  -- Not using 'finally' et al on purpose. See comment below.
  takeMVar before
  result <- action
  putMVar after ()
  pure result


--------------------------------------------------------------------------------
-- The pipeline mechanism, in ascii.
--
-- At the beginning of time:
--
-- -Global-----------+-T1---------------
--                   |
-- +sync+  +relay-+  |
-- | () |  |+----+|  |
-- +----+  || () ||  |
--         |+----+|  |
--         +------+  |
--
-- The first thread (T1) comes along and wishes to send a request. First, it
-- acquires the 'sync' lock:
--
-- -Global-----------+-T1---------------
--                   |
-- +sync+  +relay-+  | ()
-- |    |  |+----+|  |
-- +----+  || () ||  |
--         |+----+|  |
--         +------+  |
--
-- Next, it sends the request, then creates a new empty MVar that it will put to
-- when it's done receiving its response.
--
-- -Global-----------+-T1---------------
--                   |
-- +sync+  +relay-+  | () +T1--+
-- |    |  |+----+|  |    |    |
-- +----+  || () ||  |    +----+
--         |+----+|  |
--         +------+  |
--
-- It swaps this MVar with the one inside 'relay':
--
-- -Global-----------+-T1---------------
--                   |
-- +sync+  +relay-+  | () +T1--+ +----+
-- |    |  |+T1--+|  |    |    | | () |
-- +----+  ||    ||  |    +----+ +----+
--         |+----+|  |
--         +------+  |
--
-- Then it puts back the 'sync' lock.
--
-- -Global-----------+-T1---------------
--                   |
-- +sync+  +relay-+  | +T1--+ +----+
-- | () |  |+T1--+|  | |    | | () |
-- +----+  ||    ||  | +----+ +----+
--         |+----+|  |
--         +------+  |
--
-- T1 now waits for the MVar it swapped out is full (it already is). This means
-- it's T1's turn to receive its response.
--
-- -Global-----------+-T1-(receiving)---
--                   |
-- +sync+  +relay-+  | +T1--+
-- | () |  |+T1--+|  | |    |
-- +----+  ||    ||  | +----+
--         |+----+|  |
--         +------+  |
--
-- Meanwhile, T2 comes along and wants to send a request. It acquires the 'sync'
-- lock, sends, creates and empty MVar, swaps it out of 'relay', puts the 'sync'
-- lock back, and waits for that MVar to fill.
--
-- -Global-----------+-T1-(receiving)---+-T2-(waiting-on-T1)
--                   |                  |
-- +sync+  +relay-+  | +T1--+           | +T1--+ +T2--+
-- | () |  |+T2--+|  | |    |           | |    | |    |
-- +----+  ||    ||  | +----+           | +----+ +----+
--         |+----+|  |                  |
--         +------+  |                  | // Waiting on T1
--
-- Finally, T1 finishes receiving its response and puts to its MVar. Now T2 sees
-- that it's its turn to receive. And on and on.
--
-- -Global-----------+-T1-(done!)-------+-T2-(receiving)---
--                   |                  |
-- +sync+  +relay-+  | +T1--+           | +T1--+ +T2--+
-- | () |  |+T2--+|  | | () |           | | () | |    |
-- +----+  ||    ||  | +----+           | +----+ +----+
--         |+----+|  |                  |
--         +------+  |                  |
--
-- Note the cascading failure effect: T2 relies on T1 successfully executing the
-- above algorithm. Because it would be very unusual for the underling socket
-- functions to throw a synchronous exception (protobuf decode error, which
-- means the connection is busted anyway), the only potentially concerning
-- detail here is asynchronous exceptions.
--
-- Consider if thread 1 sends, then is sniped by a 'killThread', so it will
-- never "pass the baton" to thread 2 unless the requisite exception handlers
-- are installed. Thread 2 surely doesn't *want* a connection whose receive pipe
-- will eventually contain a response to thread 1's request! This is simply the
-- price one pays by using the pipelined socket abstraction: when something goes
-- wrong, every thread participating, either with an outstanding request, or
-- else in line to send or receive on the socket, will come crashing down in one
-- way or another, probably in difficult to debug and understand ways. For a
-- less chaotic failure case, consider using the "exclusive" socket, which at
-- least localizes failures to one thread at a time.
