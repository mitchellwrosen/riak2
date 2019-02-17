module RiakServerInfo where

import Libriak.Handle (Handle)
import Libriak.Connection (ConnectionError)

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

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
  -> m (Either ConnectionError (Either ByteString ServerInfo))
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
