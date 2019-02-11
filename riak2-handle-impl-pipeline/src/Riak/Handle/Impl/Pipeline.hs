module Riak.Handle.Impl.Pipeline
  ( Handle
  , Config(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
  , Error(..)
    -- ** Re-exports
  , Endpoint(..)
  , DecodeError(..)
  ) where

import Libriak.Connection (Connection, Endpoint(..), ReceiveError(..),
                           SocketException(..), withConnection)
import Libriak.Request    (Request)
import Libriak.Response   (DecodeError(..), Response)

import qualified Libriak.Connection as Connection

import Control.Concurrent.MVar
import Control.Exception.Safe  (Exception, finally, throwIO)
import Data.Coerce             (coerce)
import Foreign.C               (CInt)


data Handle
  = Handle
  { connection :: !Connection
  , sync :: !Synchronized
  , relay :: !Relay
  , handlers :: !EventHandlers
  }

data Config
  = Config
  { endpoint :: !Endpoint
  , handlers :: !EventHandlers
  }

data EventHandlers
  = EventHandlers
  { onSend :: Request -> IO ()
  , onReceive :: Either Error Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)

data Error
  = RemoteShutdown
  | SendError !CInt
  | ReceiveError !CInt
  deriving stock (Eq, Show)

-- | During connection teardown, Riak unexpectedly sent more data.
data RemoteNotShutdown
  = RemoteNotShutdown
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Acquire a handle.
--
-- /Throws/. If, during connection teardown, Riak unexpectedly sends more data,
-- throws 'RemoteNotShutdown'.
withHandle ::
     Config
  -> (Handle -> IO a)
  -> IO (Either CInt a)
withHandle Config { endpoint, handlers } k = do
  sync :: Synchronized <-
    newSynchronized

  relay :: Relay <-
    newRelay

  result <-
    withConnection endpoint $ \connection ->
      k Handle
        { connection = connection
        , sync = sync
        , relay = relay
        , handlers = handlers
        }

  case result of
    Left err ->
      socketExceptionToConnectError err

    Right value ->
      pure (Right value)

-- | Send a request.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
send ::
     Handle
  -> Request
  -> IO (Either Error ())
send Handle { connection, handlers } request = do
  onSend handlers request

  Connection.send connection request >>= \case
    Left err ->
      socketExceptionToError SendError err

    Right () ->
      pure (Right ())

-- | Receive a response.
--
-- /Throws/. If response decoding fails, throws 'DecodeError'.
receive ::
     Handle -- ^
  -> IO (Either Error Response)
receive Handle { connection, handlers } =
  Connection.receive connection >>= \case
    Left (ReceiveErrorSocket err) -> do
      response <- socketExceptionToError ReceiveError err
      onReceive handlers response
      pure response

    Left (ReceiveErrorDecode err) ->
      throwIO err

    Right (Right -> response) -> do
      onReceive handlers response
      pure response


-- | Send a request and receive the response (a single message).
--
-- /Throws/. If Riak closes the connection, throws 'RemoteShutdown'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO (Either Error Response)
exchange handle@(Handle { sync, relay }) request = do
  synchronized sync doSend >>= \case
    Left err ->
      pure (Left err)

    Right baton ->
      withBaton baton (receive handle)

  where
    doSend :: IO (Either Error Baton)
    doSend =
      send handle request >>= \case
        Left err ->
          pure (Left err)

        Right () ->
          Right <$> enterRelay relay


-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either Error r)
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
    consume :: x -> IO (Either Error r)
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

socketExceptionToConnectError :: SocketException -> IO (Either CInt a)
socketExceptionToConnectError = \case
  SocketException _ (Connection.ErrorCode errno) ->
    pure (Left errno)

  SocketException _ Connection.RemoteNotShutdown ->
    throwIO RemoteNotShutdown

  SocketException _ Connection.MessageTruncated{} -> undefined
  SocketException _ Connection.NegativeBytesRequested -> undefined
  SocketException _ Connection.OptionValueSize -> undefined
  SocketException _ Connection.RemoteShutdown -> undefined
  SocketException _ Connection.SocketAddressFamily -> undefined
  SocketException _ Connection.SocketAddressSize -> undefined

socketExceptionToError ::
     (CInt -> Error)
  -> SocketException
  -> IO (Either Error a)
socketExceptionToError fromErrno = \case
  SocketException _ Connection.RemoteShutdown ->
    pure (Left RemoteShutdown)

  SocketException _ (Connection.ErrorCode errno) ->
    pure (Left (fromErrno errno))

  SocketException _ Connection.RemoteNotShutdown -> undefined
  SocketException _ Connection.MessageTruncated{} -> undefined
  SocketException _ Connection.NegativeBytesRequested -> undefined
  SocketException _ Connection.OptionValueSize -> undefined
  SocketException _ Connection.SocketAddressFamily -> undefined
  SocketException _ Connection.SocketAddressSize -> undefined


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
