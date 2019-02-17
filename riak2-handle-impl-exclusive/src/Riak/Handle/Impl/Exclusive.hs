module Riak.Handle.Impl.Exclusive
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


data Handle
  = Handle
  { connection :: !Connection
  , lock :: !(MVar ())
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
-- /Throws/. This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either ConnectError a)
withHandle HandleConfig { endpoint, handlers } onSuccess = do
  lock :: MVar () <-
    newMVar ()

  withConnection endpoint $ \connection ->
    onSuccess Handle
      { connection = connection
      , lock = lock
      , handlers = handlers
      }

-- | Send a request.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
send ::
     Handle
  -> Request
  -> IO (Either ConnectionError ())
send Handle { connection, handlers } request = do
  onSend handlers request
  Connection.send connection request

-- | Receive a response.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
receive ::
     Handle -- ^
  -> IO (Either ConnectionError Response)
receive Handle { connection, handlers } = do
  response <- Connection.receive connection
  onReceive handlers response
  pure response


-- | Send a request and receive the response (a single message).
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO (Either ConnectionError Response)
exchange handle request =
  withMVar (lock handle) $ \_ ->
    send handle request >>= \case
      Left err ->
        pure (Left err)

      Right () ->
        receive handle

-- | Send a request and stream the response (one or more messages).
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either ConnectionError r)
stream handle request value0 step =
  withMVar (lock handle) $ \_ ->
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
