{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables,
             TypeApplications #-}

module Riak
  ( Handle
  , withHandle
    -- ** Basic server info
  , ping
  , getServerInfo
    -- ** Key/value object storage
  , fetchObject
  , storeObject
  , deleteObject
    -- ** Data type storage
  , fetchDataType
  , updateDataType
    -- ** Bucket metadata
  , getBucketTypeProps
  , setBucketTypeProps
  , getBucketProps
  , setBucketProps
  , resetBucketProps
    -- ** Aggregration and search
  , listBuckets
  , listKeys
  , mapReduce

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
  liftIO (emptyResponse @RpbPingResp (exchange conn RpbPingReq))

getServerInfo
  :: MonadIO m
  => Handle
  -> m (Either RpbErrorResp RpbGetServerInfoResp)
getServerInfo (Handle conn) =
  liftIO (exchange conn RpbGetServerInfoReq)

getBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketTypeReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketTypeProps (Handle conn) req =
  liftIO (exchange conn req)

setBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketTypeReq
  -> m (Either RpbErrorResp ())
setBucketTypeProps (Handle conn) req =
  liftIO (emptyResponse @RpbSetBucketTypeResp (exchange conn req))

getBucketProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketProps (Handle conn) req =
  liftIO (exchange conn req)

setBucketProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketReq
  -> m (Either RpbErrorResp ())
setBucketProps (Handle conn) req =
  liftIO (emptyResponse @RpbSetBucketResp (exchange conn req))

resetBucketProps
  :: MonadIO m
  => Handle
  -> RpbResetBucketReq
  -> m (Either RpbErrorResp ())
resetBucketProps (Handle conn) req =
  liftIO (emptyResponse @RpbResetBucketResp (exchange conn req))

fetchObject
  :: MonadIO m
  => Handle
  -> RpbGetReq
  -> m (Either RpbErrorResp RpbGetResp)
fetchObject (Handle conn) req =
  liftIO (exchange conn req)

storeObject
  :: MonadIO m
  => Handle
  -> RpbPutReq
  -> m (Either RpbErrorResp RpbPutResp)
storeObject (Handle conn) req =
  liftIO (exchange conn req)

deleteObject
  :: MonadIO m
  => Handle
  -> RpbDelReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteObject (Handle conn) req =
  liftIO (exchange conn req)

fetchDataType
  :: MonadIO m
  => Handle
  -> DtFetchReq
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn) req =
  liftIO (exchange conn req)

updateDataType
  :: MonadIO m
  => Handle
  -> DtUpdateReq
  -> m (Either RpbErrorResp DtUpdateResp)
updateDataType (Handle conn) req =
  liftIO (exchange conn req)

listBuckets
  :: MonadIO m
  => Handle
  -> RpbListBucketsReq
  -> m (Either RpbErrorResp RpbListBucketsResp)
listBuckets (Handle conn) req =
  liftIO (exchange conn req)

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

mapReduce
  :: MonadIO m
  => Handle
  -> RpbMapRedReq
  -> m (Either RpbErrorResp [RpbMapRedResp])
mapReduce (Handle conn) req = liftIO $ do
  send conn req

  let
    loop :: ExceptT RpbErrorResp IO [RpbMapRedResp]
    loop = do
      resp :: RpbMapRedResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. done
        then pure [resp]
        else (resp :) <$> loop

  runExceptT loop

emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)
