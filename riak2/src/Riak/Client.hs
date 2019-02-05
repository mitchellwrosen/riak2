module Riak.Client
  ( Client
  , new
  , ping
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
  -> m (Either Text ())
ping client = liftIO $
  exchange
    client
    (RequestPing defMessage)
    (\case
      ResponsePing _ -> Just ()
      _ -> Nothing)
