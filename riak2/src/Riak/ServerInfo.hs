module Riak.ServerInfo
  ( ServerInfo(..)
  , getServerInfo
  ) where

import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude

import qualified Riak.Interface  as Interface
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L


data ServerInfo
  = ServerInfo
  { name :: !Text
  , version :: !Text
  } deriving stock (Show)

-- | Get server info.
getServerInfo ::
     MonadIO m
  => Client -- ^
  -> m (Either ByteString ServerInfo)
getServerInfo client = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getServerInfo client)

  where
    fromResponse :: Proto.GetServerInfoResponse -> ServerInfo
    fromResponse response =
      ServerInfo
        { name = decodeUtf8 (response ^. L.node)
        , version = decodeUtf8 (response ^. L.version)
        }
