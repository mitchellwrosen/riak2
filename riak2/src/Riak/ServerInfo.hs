module Riak.ServerInfo
  ( ServerInfo(..)
  , get
  ) where

import Riak.Internal.Client  (Client, Error)
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L

import Data.Text.Encoding (decodeUtf8)


data ServerInfo
  = ServerInfo
  { name :: !Text
  , version :: !Text
  } deriving stock (Show)

-- | Get server info.
get ::
     MonadIO m
  => Client -- ^
  -> m (Either Error ServerInfo)
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
        { name = decodeUtf8 (response ^. L.node)
        , version = decodeUtf8 (response ^. L.version)
        }
