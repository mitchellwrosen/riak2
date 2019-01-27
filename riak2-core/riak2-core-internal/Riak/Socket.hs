module Riak.Socket
  ( Socket(..)
  , Socket.new1
  , Socket.new2
  , Socket.new3
  , Socket.connect
  , Socket.disconnect
  , send
  , receive
  ) where

import Riak.Request  (Request)
import Riak.Response (Response)
import Socket        (Socket)

import qualified Riak.Request  as Request
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
