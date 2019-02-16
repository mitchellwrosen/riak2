module Riak.Handle.Impl.Exclusive
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
  , HandleError(..)
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
import Control.Exception       (Exception, throwIO)
import Foreign.C               (CInt)


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
  , onReceive :: Either HandleError Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)

data HandleError
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
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either CInt a)
withHandle HandleConfig { endpoint, handlers } k = do
  lock :: MVar () <-
    newMVar ()

  result <-
    withConnection endpoint $ \connection ->
      k Handle
        { connection = connection
        , lock = lock
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
  -> IO (Either HandleError ())
send Handle { connection, handlers } request = do
  onSend handlers request

  Connection.send connection request >>= \case
    Left err ->
      socketExceptionToHandleError SendError err

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
    Left (ReceiveErrorSocket err) -> do
      response <- socketExceptionToHandleError ReceiveError err
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
  -> IO (Either HandleError Response)
exchange handle request =
  withMVar (lock handle) $ \_ ->
    send handle request >>= \case
      Left err ->
        pure (Left err)

      Right () ->
        receive handle

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either HandleError r)
stream handle request value0 step =
  withMVar (lock handle) $ \_ ->
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

socketExceptionToHandleError ::
     (CInt -> HandleError)
  -> SocketException
  -> IO (Either HandleError a)
socketExceptionToHandleError fromErrno = \case
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
