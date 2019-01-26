module Riak.Socket
  ( Socket
  , Socket.new
  , Socket.connect
  , Socket.disconnect
  , send
  , receive
  , Socket.HostName
  , Socket.PortNumber
  ) where

import Socket (Socket)
import Riak.Request (Request)
import Riak.Response (Response)

import qualified Riak.Request as Request
import qualified Riak.Response as Response
import qualified Socket

import Control.Exception (throwIO)

send :: Socket -> Request -> IO ()
send socket request =
  Socket.send socket (Request.encode request)

receive :: Socket -> IO (Maybe Response)
receive socket =
  Socket.receive socket >>= \case
    Nothing ->
      pure Nothing

    Just bytes ->
      case Response.parse bytes of
        Left err ->
          throwIO err

        Right response ->
          pure (Just response)
