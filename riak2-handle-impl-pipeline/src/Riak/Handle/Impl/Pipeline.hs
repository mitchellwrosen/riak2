module Riak.Handle.Impl.Pipeline
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
  , HandleConnectError(..)
  , HandleError(..)
    -- ** Re-exports
  , Endpoint(..)
  , DecodeError(..)
  ) where

import Libriak.Connection (ConnectException(..), Connection, Endpoint(..),
                           Interruptibility(..), ReceiveError(..),
                           ReceiveException(..), SendException(..),
                           withConnection)
import Libriak.Request    (Request)
import Libriak.Response   (DecodeError(..), Response)

import qualified Libriak.Connection as Connection

import Control.Concurrent.MVar
import Control.Exception.Safe  (finally, throwIO)
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
  , onReceive :: Either HandleError Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)

data HandleError
  = LocalShutdown -- ^ The socket write channel is shut down.
  | RemoteReset -- ^ The remote peer reset the connection.
  | RemoteShutdown -- ^ The remote peer's write channel is shut down.
  deriving stock (Eq, Show)

data HandleConnectError
  = ConnectionFirewalled
  | ConnectionRefused
  | ConnectionTimedOut
  | NetworkUnreachable
  | NoEphemeralPortsAvailable
  | TooManyOpenFiles
  deriving stock (Eq, Show)


-- | Acquire a handle.
--
-- /Throws/. This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either HandleConnectError a)
withHandle HandleConfig { endpoint, handlers } onSuccess = do
  sync :: Synchronized <-
    newSynchronized

  relay :: Relay <-
    newRelay

  result <-
    withConnection endpoint (const pure) $ \connection ->
      onSuccess Handle
        { connection = connection
        , sync = sync
        , relay = relay
        , handlers = handlers
        }

  case result of
    Left err ->
      pure (Left (fromConnectException err))

    Right value ->
      pure (Right value)

-- | Send a request.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
send ::
     Handle
  -> Request
  -> IO (Either HandleError ())
send Handle { connection, handlers } request = do
  onSend handlers request

  Connection.send connection request >>= \case
    Left err ->
      pure (Left (fromSendException err))

    Right () ->
      pure (Right ())

-- | Receive a response.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
receive ::
     Handle -- ^
  -> IO (Either HandleError Response)
receive Handle { connection, handlers } =
  Connection.receive connection >>= \case
    Left (ReceiveErrorSocket recvErr) -> do
      let
        response :: Either HandleError Response
        response =
          Left (fromReceiveException recvErr)

      onReceive handlers response
      pure response

    Left (ReceiveErrorDecode err) ->
      throwIO err

    Right (Right -> response) -> do
      onReceive handlers response
      pure response


-- | Send a request and receive the response (a single message).
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO (Either HandleError Response)
exchange handle@(Handle { sync, relay }) request = do
  synchronized sync doSend >>= \case
    Left err ->
      pure (Left err)

    Right baton ->
      withBaton baton (receive handle)

  where
    doSend :: IO (Either HandleError Baton)
    doSend =
      send handle request >>= \case
        Left err ->
          pure (Left err)

        Right () ->
          Right <$> enterRelay relay


-- | Send a request and stream the response (one or more messages).
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either HandleError r)
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
    consume :: x -> IO (Either HandleError r)
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


fromConnectException ::
     ConnectException 'Uninterruptible
  -> HandleConnectError
fromConnectException = \case
  ConnectEphemeralPortsExhausted -> NoEphemeralPortsAvailable
  ConnectFileDescriptorLimit     -> TooManyOpenFiles
  ConnectFirewalled              -> ConnectionFirewalled
  ConnectNetworkUnreachable      -> NetworkUnreachable
  ConnectRefused                 -> ConnectionRefused
  ConnectTimeout                 -> ConnectionTimedOut

fromSendException :: SendException 'Uninterruptible -> HandleError
fromSendException = \case
  SendReset    -> RemoteReset
  SendShutdown -> LocalShutdown

fromReceiveException :: ReceiveException 'Uninterruptible -> HandleError
fromReceiveException = \case
  ReceiveShutdown -> RemoteShutdown


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
