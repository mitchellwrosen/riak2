module Riak.Handle.Impl.Exclusive
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , withHandle
  , exchange
  , stream
  , HandleSetupError(..)
  , HandleError(..)
  , HandleTeardownError
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
import Control.Exception       (throwIO)


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
  = LocalShutdown -- ^ The socket write channel is shut down.
  | RemoteReset -- ^ The remote peer reset the connection.
  | RemoteShutdown -- ^ The remote peer's write channel is shut down.
  deriving stock (Eq, Show)

data HandleSetupError
  = ConnectionFirewalled
  | ConnectionRefused
  | ConnectionTimedOut
  | NetworkUnreachable
  | NoEphemeralPortsAvailable
  | TooManyOpenFiles
  deriving stock (Eq, Show)

data HandleTeardownError
  deriving stock (Eq, Show)


-- | Acquire a handle.
--
-- /Throws/. This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Maybe HandleTeardownError -> a -> IO b)
  -> (Handle -> IO a)
  -> IO (Either HandleSetupError b)
withHandle HandleConfig { endpoint, handlers } onTeardown onSuccess = do
  lock :: MVar () <-
    newMVar ()

  result <-
    withConnection endpoint (\_ -> onTeardown Nothing) $ \connection ->
      onSuccess Handle
        { connection = connection
        , lock = lock
        , handlers = handlers
        }

  case result of
    Left err ->
      pure (Left (connectErrorToSetupError err))

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
      pure (Left (sendErrorToHandleError err))

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
          Left (recvErrorToHandleError recvErr)

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

connectErrorToSetupError ::
     ConnectException 'Uninterruptible
  -> HandleSetupError
connectErrorToSetupError = \case
  ConnectEphemeralPortsExhausted -> NoEphemeralPortsAvailable
  ConnectFileDescriptorLimit     -> TooManyOpenFiles
  ConnectFirewalled              -> ConnectionFirewalled
  ConnectNetworkUnreachable      -> NetworkUnreachable
  ConnectRefused                 -> ConnectionRefused
  ConnectTimeout                 -> ConnectionTimedOut

sendErrorToHandleError :: SendException 'Uninterruptible -> HandleError
sendErrorToHandleError = \case
  SendReset    -> RemoteReset
  SendShutdown -> LocalShutdown

recvErrorToHandleError :: ReceiveException 'Uninterruptible -> HandleError
recvErrorToHandleError = \case
  ReceiveShutdown -> RemoteShutdown
