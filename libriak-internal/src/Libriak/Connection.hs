-- | A low-level connection to Riak.
--
-- Like the underlying socket, this is not a thread-safe abstraction.

module Libriak.Connection
  ( Connection
  , withConnection
  , send
  , receive
  , ConnectError(..)
  , ConnectionError(..)
    -- * Re-exports
  , DecodeError(..)
  , Endpoint(..)
  ) where

import Libriak.Internal.Connection (ConnectException(..), Connection,
                                    Endpoint(..), Interruptibility(..),
                                    ReceiveException(..), SendException(..))
import Libriak.Request             (Request, encodeRequest)
import Libriak.Response            (DecodeError(..), Response, parseResponse)

import qualified Libriak.Internal.Connection as Connection

import Control.Exception (throwIO)
import Data.Bifunctor    (first)


data ConnectError
  = ConnectionFirewalled
  | ConnectionRefused
  | ConnectionTimedOut
  | NetworkUnreachable
  | NoEphemeralPortsAvailable
  | TooManyOpenFiles
  deriving stock (Eq, Show)

data ConnectionError
  = LocalShutdown -- ^ The socket write channel is shut down.
  | RemoteReset -- ^ The remote peer reset the connection.
  | RemoteShutdown -- ^ The remote peer's write channel is shut down.
  deriving stock (Eq, Show)

-- | Acquire a connection.
--
-- /Throws/. This function will never throw an exception.
withConnection ::
     Endpoint
  -> (Connection -> IO a)
  -> IO (Either ConnectError a)
withConnection endpoint onSuccess =
  first fromConnectException <$>
    Connection.withConnection endpoint (const pure) onSuccess

-- | Send a request.
--
-- /Throws/. This function will never throw an exception.
send ::
     Connection
  -> Request
  -> IO (Either ConnectionError ())
send conn request =
  first fromSendException <$>
    Connection.send conn (encodeRequest request)

-- | Receive a response.
--
-- /Throws/. If response decoding fails, which should never happen, throws
-- 'DecodeError'.
receive ::
     Connection
  -> IO (Either ConnectionError Response)
receive conn =
  Connection.receive conn >>= \case
    Left err ->
      pure (Left (fromReceiveException err))

    Right bytes ->
      case parseResponse bytes of
        Left err ->
          throwIO err

        Right response ->
          pure (Right response)

fromConnectException ::
     ConnectException 'Uninterruptible
  -> ConnectError
fromConnectException = \case
  ConnectEphemeralPortsExhausted -> NoEphemeralPortsAvailable
  ConnectFileDescriptorLimit     -> TooManyOpenFiles
  ConnectFirewalled              -> ConnectionFirewalled
  ConnectNetworkUnreachable      -> NetworkUnreachable
  ConnectRefused                 -> ConnectionRefused
  ConnectTimeout                 -> ConnectionTimedOut

fromSendException :: SendException 'Uninterruptible -> ConnectionError
fromSendException = \case
  SendReset    -> RemoteReset
  SendShutdown -> LocalShutdown

fromReceiveException :: ReceiveException 'Uninterruptible -> ConnectionError
fromReceiveException = \case
  ReceiveReset -> RemoteReset
  ReceiveShutdown -> RemoteShutdown
