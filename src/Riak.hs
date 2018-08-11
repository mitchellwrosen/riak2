{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables,
             TypeApplications #-}

module Riak
  ( -- * Riak handle
    Handle
  , withHandle
    -- * Key/value object operations
  , fetchObject
  , storeObject
  , deleteObject
    -- * Data type operations
  , fetchDataType
  , updateDataType
    -- * Bucket operations
  , getBucketTypeProps
  , setBucketTypeProps
  , getBucketProps
  , setBucketProps
  , resetBucketProps
  , listBuckets
  , listKeys
    -- * MapReduce
  , mapReduce
    -- * Secondary indexes (2i)
    -- * Search 2.0
  , getSchema
  , putSchema
  , getIndex
  , putIndex
  , deleteIndex
    -- * Server info
  , ping
  , getServerInfo
    -- * Re-exports
  , def
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Default.Class         (def)
import Data.HashMap.Strict        (HashMap)
import Data.IORef
import Lens.Family2
import Network.Socket             (HostName, PortNumber)

import qualified Data.HashMap.Strict as HashMap

import Proto.Riak
import qualified Proto.Riak_Fields as L
import Riak.Internal.Connection
import Riak.Internal.Request
import Riak.Internal.Response

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Connection
      !(IORef (HashMap (Maybe ByteString, ByteString, ByteString) ByteString))

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Handle -> m a)
  -> m a
withHandle host port f = do
  vclockCacheRef <-
    liftIO (newIORef mempty)

  withConnection host port (\conn -> f (Handle conn vclockCacheRef))

fetchObject
  :: MonadIO m
  => Handle
  -> RpbGetReq
  -> m (Either RpbErrorResp (Maybe RpbGetResp))
fetchObject (Handle conn vclockCacheRef) req = liftIO $
  exchange conn (req & L.deletedvclock .~ True) >>= \case
    Left err ->
      pure (Left err)

    Right resp -> do
      modifyIORef'
        vclockCacheRef
        (maybe
          (HashMap.delete (type', bucket, key))
          (HashMap.insert (type', bucket, key))
          (resp ^. L.maybe'vclock))

      case resp ^. L.content of
        [] ->
          pure (Right Nothing)
        _ ->
          pure (Right (Just resp))

 where
  type'  = req ^. L.maybe'type'
  bucket = req ^. L.bucket
  key    = req ^. L.key

storeObject
  :: MonadIO m
  => Handle
  -> RpbPutReq
  -> m (Either RpbErrorResp RpbPutResp)
storeObject (Handle conn _) req =
  liftIO (exchange conn req)

deleteObject
  :: MonadIO m
  => Handle
  -> RpbDelReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteObject (Handle conn _) req =
  liftIO (exchange conn req)

fetchDataType
  :: MonadIO m
  => Handle
  -> DtFetchReq
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn _) req =
  liftIO (exchange conn req)

updateDataType
  :: MonadIO m
  => Handle
  -> DtUpdateReq
  -> m (Either RpbErrorResp DtUpdateResp)
updateDataType (Handle conn _) req =
  liftIO (exchange conn req)

getBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketTypeReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketTypeProps (Handle conn _) req =
  liftIO (exchange conn req)

setBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketTypeReq
  -> m (Either RpbErrorResp ())
setBucketTypeProps (Handle conn _) req =
  liftIO (emptyResponse @RpbSetBucketTypeResp (exchange conn req))

getBucketProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketProps (Handle conn _) req =
  liftIO (exchange conn req)

setBucketProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketReq
  -> m (Either RpbErrorResp ())
setBucketProps (Handle conn _) req =
  liftIO (emptyResponse @RpbSetBucketResp (exchange conn req))

resetBucketProps
  :: MonadIO m
  => Handle
  -> RpbResetBucketReq
  -> m (Either RpbErrorResp ())
resetBucketProps (Handle conn _) req =
  liftIO (emptyResponse @RpbResetBucketResp (exchange conn req))


listBuckets
  :: MonadIO m
  => Handle
  -> RpbListBucketsReq
  -> m (Either RpbErrorResp RpbListBucketsResp)
listBuckets (Handle conn _) req =
  liftIO (exchange conn req)

-- TODO streaming listKeys
-- TODO key newtype
listKeys
  :: MonadIO m
  => Handle
  -> RpbListKeysReq
  -> m (Either RpbErrorResp [ByteString])
listKeys (Handle conn _) req = liftIO $ do
  send conn req

  let
    loop :: ExceptT RpbErrorResp IO [ByteString]
    loop = do
      resp :: RpbListKeysResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. L.done
        then pure (resp ^. L.keys)
        else ((resp ^. L.keys) ++) <$> loop

  runExceptT loop


mapReduce
  :: MonadIO m
  => Handle
  -> RpbMapRedReq
  -> m (Either RpbErrorResp [RpbMapRedResp])
mapReduce (Handle conn _) req = liftIO $ do
  send conn req

  let
    loop :: ExceptT RpbErrorResp IO [RpbMapRedResp]
    loop = do
      resp :: RpbMapRedResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. L.done
        then pure [resp]
        else (resp :) <$> loop

  runExceptT loop

getSchema
  :: MonadIO m
  => Handle
  -> RpbYokozunaSchemaGetReq
  -> m (Either RpbErrorResp RpbYokozunaSchemaGetResp)
getSchema (Handle conn _) req =
  liftIO (exchange conn req)

putSchema
  :: MonadIO m
  => Handle
  -> RpbYokozunaSchemaPutReq
  -> m (Either RpbErrorResp RpbEmptyPutResp)
putSchema (Handle conn _) req =
  liftIO (exchange conn req)

getIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexGetReq
  -> m (Either RpbErrorResp RpbYokozunaIndexGetResp)
getIndex (Handle conn _) req =
  liftIO (exchange conn req)

putIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexPutReq
  -> m (Either RpbErrorResp RpbEmptyPutResp)
putIndex (Handle conn _) req =
  liftIO (exchange conn req)

deleteIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexDeleteReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteIndex (Handle conn _) req =
  liftIO (exchange conn req)

ping :: MonadIO m => Handle -> m (Either RpbErrorResp ())
ping (Handle conn _) =
  liftIO (emptyResponse @RpbPingResp (exchange conn RpbPingReq))

getServerInfo
  :: MonadIO m
  => Handle
  -> m (Either RpbErrorResp RpbGetServerInfoResp)
getServerInfo (Handle conn _) =
  liftIO (exchange conn RpbGetServerInfoReq)

emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)
