-- | A low-level connection to Riak.
--
-- Like the underlying socket, this is not a thread-safe abstraction.

module Libriak.Connection
  ( Connection
  , withConnection
  , send
  , receive
  , ReceiveError(..)
    -- * Re-exports
  , DecodeError(..)
  , Endpoint(..)
  , ConnectException(..)
  , SendException(..)
  , ReceiveException(..)
  , CloseException(..)
  , Interruptibility(..)
  ) where

import Libriak.Internal.Connection (CloseException(..), ConnectException(..),
                                    Connection, Endpoint(..),
                                    Interruptibility(..), ReceiveException(..),
                                    SendException(..), withConnection)
import Libriak.Request             (Request, encodeRequest)
import Libriak.Response            (DecodeError(..), Response, parseResponse)

import qualified Libriak.Internal.Connection as Connection

-- | Something went wrong while receiving a response.
--
-- TODO Get rid of this type and just throwIO decode errors?
data ReceiveError
  = ReceiveErrorSocket (ReceiveException 'Uninterruptible)
  | ReceiveErrorDecode DecodeError
  deriving stock (Show)

-- | Send a request.
--
-- /Throws/. This function will never throw an exception.
send ::
     Connection
  -> Request
  -> IO (Either (SendException 'Uninterruptible) ())
send conn request =
  Connection.send conn (encodeRequest request)

-- | Receive a response.
--
-- /Throws/. This function will never throw an exception.
receive ::
     Connection
  -> IO (Either ReceiveError Response)
receive conn =
  Connection.receive conn >>= \case
    Left err ->
      pure (Left (ReceiveErrorSocket err))

    Right bytes ->
      case parseResponse bytes of
        Left err ->
          pure (Left (ReceiveErrorDecode err))

        Right response ->
          pure (Right response)
