{-# LANGUAGE DataKinds, DerivingStrategies, GeneralizedNewtypeDeriving,
             LambdaCase, ScopedTypeVariables, TypeApplications,
             TypeOperators #-}

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
    -- * Optional parameters
  , (:=)((:=))
  , param
    -- * Types
  , Bucket(..)
  , BucketType(..)
  , Key(..)
    -- * Re-exports
  , Proxy(..)
  , def
  , (&)
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Coerce                (coerce)
import Data.Default.Class         (def)
import Data.Hashable              (Hashable)
import Data.HashMap.Strict        (HashMap)
import Data.IORef
import Data.Proxy                 (Proxy(Proxy))
import Data.Word
import Lens.Family2
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head)

import qualified Data.HashMap.Strict as HashMap

import           Proto.Riak
import qualified Proto.Riak_Fields        as L
import           Riak.Internal.Connection
import           Riak.Internal.Param
import           Riak.Internal.Request
import           Riak.Internal.Response

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Connection
      !(IORef (HashMap (Maybe BucketType, Bucket, Key) ByteString))

newtype BucketType
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

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
  -> Bucket
  -> Key
  -> ( "basic_quorum"  := Bool
     , "head"          := Bool
     , "if_modified"   := ByteString
     , "n_val"         := Word32
     , "notfound_ok"   := Bool
     , "pr"            := Word32
     , "r"             := Word32
     , "sloppy_quorum" := Bool
     , "timeout"       := Word32
     , "type"          := BucketType
     )
  -> m (Either RpbErrorResp (Maybe RpbGetResp))
fetchObject
    (Handle conn vclockCacheRef) bucket key
    ( _ := basic_quorum
    , _ := head
    , _ := if_modified
    , _ := n_val
    , _ := notfound_ok
    , _ := pr
    , _ := r
    , _ := sloppy_quorum
    , _ := timeout
    , _ := type'
    ) = liftIO $
  exchange conn request >>= \case
    Left err ->
      pure (Left err)

    -- TODO; this logic assumes if_modified is not set. If it is, and the object
    -- is up to date, then we will have content = [], vclock = Nothing,
    -- unchanged = Just True.
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
  request :: RpbGetReq
  request =
    RpbGetReq
      { _RpbGetReq'_unknownFields = []
      , _RpbGetReq'basicQuorum    = basic_quorum
      , _RpbGetReq'bucket         = unBucket bucket
      , _RpbGetReq'deletedvclock  = Just True
      , _RpbGetReq'head           = head
      , _RpbGetReq'ifModified     = if_modified
      , _RpbGetReq'key            = unKey key
      , _RpbGetReq'nVal           = n_val
      , _RpbGetReq'notfoundOk     = notfound_ok
      , _RpbGetReq'pr             = pr
      , _RpbGetReq'r              = r
      , _RpbGetReq'sloppyQuorum   = sloppy_quorum
      , _RpbGetReq'timeout        = timeout
      , _RpbGetReq'type'          = coerce type'
      }

storeObject
  :: MonadIO m
  => Handle
  -> RpbPutReq
  -> m (Either RpbErrorResp RpbPutResp)
storeObject handle req =
  liftIO (runExceptT (storeObject_ handle req))

storeObject_
  :: Handle
  -> RpbPutReq
  -> ExceptT RpbErrorResp IO RpbPutResp
storeObject_ handle@(Handle conn vclockCacheRef) req = do
  -- Get the cached vclock of this object to pass in the put request. If we
  -- don't have it, first perform a head-fetch, which caches it. If it's still
  -- not there, then this must be the very first store.
  vclock :: Maybe ByteString <-
    lift lookupVclock >>= \case
      Nothing -> do
        _ <- ExceptT (fetchObject handle (Bucket bucket) (Key key) params)
        lift lookupVclock
      Just vclock ->
        pure (Just vclock)

  ExceptT (exchange conn (req & L.maybe'vclock .~ vclock))

 where
  lookupVclock :: IO (Maybe ByteString)
  lookupVclock =
    HashMap.lookup (coerce type', Bucket bucket, Key key) <$>
      readIORef vclockCacheRef

  params
    :: ( "basic_quorum"  := Bool
       , "head"          := Bool
       , "if_modified"   := ByteString
       , "n_val"         := Word32
       , "notfound_ok"   := Bool
       , "pr"            := Word32
       , "r"             := Word32
       , "sloppy_quorum" := Bool
       , "timeout"       := Word32
       , "type"          := BucketType
       )
  params =
    def
      & param (Proxy @"head") True
      & case type' of
          Nothing ->
            id
          Just type'' ->
            param (Proxy @"type") (BucketType type'')

  type'  = req ^. L.maybe'type'
  bucket = req ^. L.bucket
  key    = req ^. L.key

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