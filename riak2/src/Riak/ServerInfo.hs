module Riak.ServerInfo
  ( ServerInfo(..)
  , getServerInfo
  ) where

import Riak.Handle           (Handle)
import Riak.Internal.Prelude

import qualified Riak.Handle     as Handle
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)


data ServerInfo
  = ServerInfo
  { name :: !Text
  , version :: !Text
  } deriving stock (Eq, Generic, Show)

-- | Get server info.
getServerInfo ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.Error ServerInfo)
getServerInfo handle = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.getServerInfo handle)

  where
    fromResponse :: Proto.GetServerInfoResponse -> ServerInfo
    fromResponse response =
      ServerInfo
        { name = decodeUtf8 (response ^. L.node)
        , version = decodeUtf8 (response ^. L.version)
        }
