-- | A low-level connection to Riak.
--
-- Like the underlying socket, this is not a thread-safe abstraction.

module Riak.Connection
  ( Connection
  , withConnection
  , send
  , receive
  , ReceiveError(..)
    -- * Re-exports
  , DecodeError(..)
  , Endpoint(..)
  , SocketException(..)
  , Context(..)
  , Reason(..)
  ) where

import Riak.Connection.Internal (Connection, Context(..), Endpoint(..),
                                 Reason(..), SocketException(..),
                                 withConnection)
import Riak.Request             (Request, encodeRequest)
import Riak.Response            (DecodeError(..), Response, parseResponse)

import qualified Riak.Connection.Internal as Connection

-- | Something went wrong while receiving a response.
data ReceiveError
  = ReceiveErrorSocket SocketException
  | ReceiveErrorDecode DecodeError

-- | Send a request.
--
-- /Throws/. This function will never throw an exception.
send :: Connection -> Request -> IO (Either SocketException ())
send conn request =
  Connection.send conn (encodeRequest request)

-- | Receive a response.
--
-- /Throws/. This function will never throw an exception.
receive :: Connection -> IO (Either ReceiveError Response)
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
