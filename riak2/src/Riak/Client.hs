module Riak.Client
  ( Client
  , new
  , ping
  , UnexpectedResponse(..)
  ) where

import Riak.Interface        (Interface, UnexpectedResponse(..))
import Riak.Internal.Prelude

import qualified Riak.Interface as Interface

-- TODO what is the point of Client? is it just Interface?
type Client
  = Interface

new :: Interface -> Client
new =
  id

-- | Ping the server.
ping ::
     MonadIO m
  => Client -- ^
  -> m (Either ByteString ())
ping client =
  liftIO (Interface.ping client)
