{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables,
             TypeApplications #-}

module Riak
  ( Handle
  , withHandle
  , ping
  , getServerInfo
  , listBuckets
  , listKeys
  , getBucketProps
  , setBucketProps
  , resetBucketProps
  , fetchObject

    -- ** Re-exports
  , def
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Default.Class         (def)
import Lens.Family2
import Network.Socket             (HostName, PortNumber)

import Proto.Riak
import Proto.Riak_Fields        (done, keys)
import Riak.Internal.Connection
import Riak.Internal.Request
import Riak.Internal.Response

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
ping (Handle conn) =
  liftIO (emptyResponse @RpbPingResp (exchange1 conn RpbPingReq))

getServerInfo
  :: MonadIO m
  => Handle
  -> m (Either RpbErrorResp RpbGetServerInfoResp)
getServerInfo (Handle conn) =
  liftIO (exchange1 conn RpbGetServerInfoReq)

listBuckets
  :: MonadIO m
  => Handle
  -> RpbListBucketsReq
  -> m (Either RpbErrorResp RpbListBucketsResp)
listBuckets (Handle conn) req =
  liftIO (exchange1 conn req)

-- TODO streaming listKeys
-- TODO key newtype
listKeys
  :: MonadIO m
  => Handle
  -> RpbListKeysReq
  -> m (Either RpbErrorResp [ByteString])
listKeys (Handle conn) req = liftIO $ do
  send conn req

  let
    loop :: ExceptT RpbErrorResp IO [ByteString]
    loop = do
      resp :: RpbListKeysResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. done
        then pure (resp ^. keys)
        else ((resp ^. keys) ++) <$> loop

  runExceptT loop

getBucketProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketProps (Handle conn) req =
  liftIO (exchange1 conn req)

setBucketProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketReq
  -> m (Either RpbErrorResp ())
setBucketProps (Handle conn) req =
  liftIO (emptyResponse @RpbSetBucketResp (exchange1 conn req))

resetBucketProps
  :: MonadIO m
  => Handle
  -> RpbResetBucketReq
  -> m (Either RpbErrorResp ())
resetBucketProps (Handle conn) req =
  liftIO (emptyResponse @RpbResetBucketResp (exchange1 conn req))

fetchObject
  :: MonadIO m
  => Handle
  -> RpbGetReq
  -> m (Either RpbErrorResp RpbGetResp)
fetchObject (Handle conn) req =
  liftIO (exchange1 conn req)

emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)
