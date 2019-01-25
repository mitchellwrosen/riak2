module Riak.ServerInfo
  ( ServerInfo(..)
  , get
  ) where

import Riak.Internal.Client  (Client, Result)
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L


-- TODO Text, Text
data ServerInfo
  = ServerInfo
  { name :: ByteString
  , version :: ByteString
  } deriving stock (Show)

get ::
     MonadIO m
  => Client
  -> m (Result ServerInfo)
get client = liftIO $
  (fmap.fmap)
    fromResponse
    (Client.exchange
      client
      (RequestGetServerInfo defMessage)
      (\case
        ResponseGetServerInfo response -> Just response
        _ -> Nothing))

  where
    fromResponse :: Proto.GetServerInfoResponse -> ServerInfo
    fromResponse response =
      ServerInfo
        { name = response ^. L.node
        , version = response ^. L.version
        }
