module Riak.Client
  ( Client
  , new
  , ping
  , Error(..)
  , UnexpectedResponse(..)
  ) where

import Riak.Internal.Client
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

-- | Ping the server.
ping ::
     MonadIO m
  => Client -- ^
  -> m (Either Error ())
ping client = liftIO $
  exchange
    client
    (RequestPing defMessage)
    (\case
      ResponsePing _ -> Just ()
      _ -> Nothing)
