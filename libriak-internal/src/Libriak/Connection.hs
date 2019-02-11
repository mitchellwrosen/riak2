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
  , SocketException(..)
  , Context(..)
  , Reason(..)
  ) where

import Libriak.Internal.Connection (Connection, Context(..), Endpoint(..),
                                    Reason(..), SocketException(..),
                                    withConnection)
import Libriak.Request             (Request, encodeRequest)
import Libriak.Response            (DecodeError(..), Response, parseResponse)

import qualified Libriak.Internal.Connection as Connection

-- | Something went wrong while receiving a response.
data ReceiveError
  = ReceiveErrorSocket SocketException
  | ReceiveErrorDecode DecodeError
  deriving stock (Show)

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
