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
import Libriak.Request             (EncodedRequest(..))
import Libriak.Response            (DecodeError(..), EncodedResponse(..))

import qualified Libriak.Internal.Connection as Connection

import Data.Bifunctor (bimap, first)


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
  -> EncodedRequest
  -> IO (Either ConnectionError ())
send conn (EncodedRequest request) =
  first fromSendException <$>
    Connection.send conn request

-- | Receive a response.
receive ::
     Connection
  -> IO (Either ConnectionError EncodedResponse)
receive =
  fmap (bimap fromReceiveException EncodedResponse) . Connection.receive

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
