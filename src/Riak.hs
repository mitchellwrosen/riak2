{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Riak
  ( Handle
  , withHandle
  , ping
  , getServerInfo
  , listBuckets
  ) where

import Control.Monad.IO.Unlift
import Network.Socket          (HostName, PortNumber)

import qualified Data.ProtoLens.Encoding as Proto (encodeMessage)

import Proto.Riak
import Riak.Internal.Connection
import Riak.Internal.Message
import Riak.Internal.Response   (RpbPingResp)

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle !Connection

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Handle -> m a)
  -> m a
withHandle host port f =
  withConnection host port (f . Handle)

ping :: MonadIO m => Handle -> m (Either RpbErrorResp ())
ping (Handle conn) = liftIO $
  f <$> exchange1 conn (Message 1 mempty)
 where
  f :: Either RpbErrorResp RpbPingResp -> Either RpbErrorResp ()
  f =
    (() <$)

getServerInfo
  :: MonadIO m
  => Handle
  -> m (Either RpbErrorResp RpbGetServerInfoResp)
getServerInfo (Handle conn) = liftIO $
  exchange1 conn (Message 7 mempty)

listBuckets
  :: MonadIO m
  => Handle
  -> RpbListBucketsReq
  -> m (Either RpbErrorResp RpbListBucketsResp)
listBuckets (Handle conn) req = liftIO $
  exchange1 conn (Message 15 (Proto.encodeMessage req))
