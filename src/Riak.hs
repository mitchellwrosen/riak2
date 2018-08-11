{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Riak
  ( Handle
  , withHandle
  , ping
  , getServerInfo
  , listBuckets
  , listKeys
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Lens.Family2
import Network.Socket             (HostName, PortNumber)

import qualified Data.ProtoLens.Encoding as Proto (encodeMessage)

import Proto.Riak
import Proto.Riak_Fields        (done, keys)
import Riak.Internal.Connection
import Riak.Internal.Message
import Riak.Internal.Response   (RpbPingResp, parseResponse)

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

-- TODO streaming listKeys
-- TODO key newtype
listKeys
  :: MonadIO m
  => Handle
  -> RpbListKeysReq
  -> m (Either RpbErrorResp [ByteString])
listKeys (Handle conn) req = liftIO $ do
  send conn (Message 17 (Proto.encodeMessage req))

  let
    loop :: ExceptT RpbErrorResp IO [ByteString]
    loop = do
      resp :: RpbListKeysResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. done
        then pure (resp ^. keys)
        else ((resp ^. keys) ++) <$> loop

  runExceptT loop
