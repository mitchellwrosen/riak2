module RiakServerInfo where

import RiakHandle (Handle, HandleError)

import qualified RiakHandle as Handle

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto


data ServerInfo
  = ServerInfo
  { name :: !Text
  , version :: !Text
  } deriving stock (Eq, Generic, Show)

-- | Get server info.
getServerInfo ::
     MonadIO m
  => Handle -- ^
  -> m (Either HandleError (Either ByteString ServerInfo))
getServerInfo handle = liftIO $
  (fmap.fmap.fmap)
    fromResponse
    (Handle.getServerInfo handle)

  where
    fromResponse :: Proto.RpbGetServerInfoResp -> ServerInfo
    fromResponse response =
      ServerInfo
        { name = decodeUtf8 (response ^. Proto.node)
        , version = decodeUtf8 (response ^. Proto.serverVersion)
        }
