{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies,
             GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures,
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
  , fetchCounter
  , fetchSet
  , fetchDataType
  , updateCounter
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
  , DataType(..)
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
import Data.Int
import Data.IORef
import Data.Proxy                 (Proxy(Proxy))
import Data.Text.Encoding         (decodeUtf8)
import Data.Word
import Lens.Family2
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head)
import UnliftIO.Exception         (Exception, throwIO)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

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
      !(IORef (HashMap (SomeBucketType, Bucket, Key) Vclock))


newtype BucketType (ty :: Maybe DataType)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)


newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Bucket where
  show :: Bucket -> String
  show =
    Text.unpack . decodeUtf8 . unBucket


data DataType
  = DataTypeCounter
  | DataTypeMap
  | DataTypeSet


-- | A 'DataTypeError' is thrown when a data type operation is performed on an
-- incompatible bucket type (for example, attempting to fetch a counter from a
-- bucket type that contains sets).
data DataTypeError
  = DataTypeError
      !SomeBucketType       -- Bucket type
      !Bucket               -- Bucket
      !Key                  -- Key
      !DtFetchResp'DataType -- Actual data type
      !DtFetchResp'DataType -- Expected data type
  deriving stock (Show)
  deriving anyclass (Exception)


newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Key where
  show :: Key -> String
  show =
    Text.unpack . decodeUtf8 . unKey


-- TODO Better Quorum type
type Quorum
  = Word32


newtype SomeBucketType
  = SomeBucketType { unSomeBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show SomeBucketType where
  show :: SomeBucketType -> String
  show =
    Text.unpack . decodeUtf8 . unSomeBucketType


newtype Vclock
  = Vclock { unVclock :: ByteString }


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


-- TODO fetchObject: better return type
fetchObject
  :: MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( "basic_quorum"  := Bool
     , "head"          := Bool
     , "if_modified"   := ByteString -- TODO use cached vclock for if_modified
     , "n_val"         := Quorum
     , "notfound_ok"   := Bool
     , "pr"            := Quorum
     , "r"             := Quorum
     , "sloppy_quorum" := Bool
     , "timeout"       := Word32
     )
  -> m (Either RpbErrorResp (Maybe RpbGetResp))
fetchObject
    handle@(Handle conn _) type' bucket key
    ( _ := basic_quorum
    , _ := head
    , _ := if_modified
    , _ := n_val
    , _ := notfound_ok
    , _ := pr
    , _ := r
    , _ := sloppy_quorum
    , _ := timeout
    ) = liftIO $
  exchange conn request >>= \case
    Left err ->
      pure (Left err)

    -- TODO; this logic assumes if_modified is not set. If it is, and the object
    -- is up to date, then we will have content = [], vclock = Nothing,
    -- unchanged = Just True.
    Right resp -> do
      cacheVclock handle type' bucket key (coerce (resp ^. L.maybe'vclock))

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
      , _RpbGetReq'bucket         = coerce bucket
      , _RpbGetReq'deletedvclock  = Just True
      , _RpbGetReq'head           = head
      , _RpbGetReq'ifModified     = if_modified
      , _RpbGetReq'key            = coerce key
      , _RpbGetReq'nVal           = n_val
      , _RpbGetReq'notfoundOk     = notfound_ok
      , _RpbGetReq'pr             = pr
      , _RpbGetReq'r              = r
      , _RpbGetReq'sloppyQuorum   = sloppy_quorum
      , _RpbGetReq'timeout        = timeout
      , _RpbGetReq'type'          = coerce (Just type')
      }


fetchObjectVclock
  :: Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ExceptT RpbErrorResp IO (Maybe Vclock)
fetchObjectVclock handle@(Handle _ vclockCacheRef) type' bucket key' =
  lift lookupVclock >>= \case
    Nothing -> do
      _ <- ExceptT (fetchObject handle type' bucket key' params) -- cache it
      lift lookupVclock

    Just vclock ->
      pure (Just vclock)

 where
  lookupVclock :: IO (Maybe Vclock)
  lookupVclock =
    HashMap.lookup (coerce type', bucket, key') <$>
      readIORef vclockCacheRef

  params
    :: ( "basic_quorum"  := Bool
       , "head"          := Bool
       , "if_modified"   := ByteString
       , "n_val"         := Quorum
       , "notfound_ok"   := Bool
       , "pr"            := Quorum
       , "r"             := Quorum
       , "sloppy_quorum" := Bool
       , "timeout"       := Word32
       )
  params =
    def & param (Proxy @"head") True


-- | Given a fetched vclock, update the cache (if present) or delete it from the
-- cache (if missing).
cacheVclock
  :: Handle
  -> BucketType ty
  -> Bucket
  -> Key
  -> Maybe Vclock
  -> IO ()
cacheVclock (Handle _ vclockCacheRef) type' bucket key vclock =
  modifyIORef'
    vclockCacheRef
    (maybe
      (HashMap.delete (coerce type', bucket, key))
      (HashMap.insert (coerce type', bucket, key))
      vclock)


-- TODO storeObject: nicer input type than RpbContent
-- TODO storeObject: better return type
storeObject
  :: MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> RpbContent
  -> ( "asis"            := Bool
     , "dw"              := Quorum
     , "if_none_match"   := Bool
     , "if_not_modified" := Bool
     , "key"             := Key
     , "n_val"           := Quorum
     , "pw"              := Quorum
     , "return_body"     := Bool
     , "return_head"     := Bool
     , "sloppy_quorum"   := Bool
     , "timeout"         := Word32
     , "w"               := Quorum
     )
  -> m (Either RpbErrorResp RpbPutResp)
storeObject handle type' bucket content params =
  liftIO (runExceptT (storeObject_ handle type' bucket content params))


storeObject_
  :: Handle
  -> BucketType 'Nothing
  -> Bucket
  -> RpbContent
  -> ( "asis"            := Bool
     , "dw"              := Quorum
     , "if_none_match"   := Bool
     , "if_not_modified" := Bool
     , "key"             := Key
     , "n_val"           := Quorum
     , "pw"              := Quorum
     , "return_body"     := Bool
     , "return_head"     := Bool
     , "sloppy_quorum"   := Bool
     , "timeout"         := Word32
     , "w"               := Quorum
     )
  -> ExceptT RpbErrorResp IO RpbPutResp
storeObject_
    handle@(Handle conn _) type' bucket content
    ( _ := asis
    , _ := dw
    , _ := if_none_match
    , _ := if_not_modified
    , _ := key
    , _ := n_val
    , _ := pw
    , _ := return_body
    , _ := return_head
    , _ := sloppy_quorum
    , _ := timeout
    , _ := w
    ) = do

  -- Get the cached vclock of this object to pass in the put request. If we
  -- don't have it, first perform a head-fetch, which caches it. If it's still
  -- not there, then this must be the very first store.
  vclock :: Maybe Vclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (fetchObjectVclock handle type' bucket)
      key

  let
    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = asis
        , _RpbPutReq'bucket         = coerce bucket
        , _RpbPutReq'content        = content
        , _RpbPutReq'dw             = dw
        , _RpbPutReq'ifNoneMatch    = if_none_match
        , _RpbPutReq'ifNotModified  = if_not_modified
        , _RpbPutReq'key            = coerce key
        , _RpbPutReq'nVal           = n_val
        , _RpbPutReq'pw             = pw
        , _RpbPutReq'returnBody     = return_body
        , _RpbPutReq'returnHead     = return_head
        , _RpbPutReq'sloppyQuorum   = sloppy_quorum
        , _RpbPutReq'timeout        = timeout
        , _RpbPutReq'type'          = coerce (Just type')
        , _RpbPutReq'vclock         = coerce vclock
        , _RpbPutReq'w              = w
        }

  ExceptT (exchange conn request)


-- TODO deleteObject figure out when vclock is required (always?)
deleteObject
  :: MonadIO m
  => Handle
  -> RpbDelReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteObject (Handle conn _) req =
  liftIO (exchange conn req)


fetchCounter
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Key
  -> ( "basic_quorum"    := Bool
     , "n_val"           := Quorum
     , "notfound_ok"     := Bool
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := Bool
     , "timeout"         := Word32
     )
  -> m (Either RpbErrorResp Int64)
fetchCounter
    (Handle conn _) type' bucket key
    ( _ := basic_quorum
    , _ := n_val
    , _ := notfound_ok
    , _ := pr
    , _ := r
    , _ := sloppy_quorum
    , _ := timeout
    ) = runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (liftIO (exchange conn request))

  case response ^. L.type' of
    DtFetchResp'COUNTER ->
      pure (response ^. L.value . L.counterValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt
          DtFetchResp'COUNTER)

 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Nothing
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = notfound_ok
      , _DtFetchReq'pr             = pr
      , _DtFetchReq'r              = r
      , _DtFetchReq'sloppyQuorum   = sloppy_quorum
      , _DtFetchReq'timeout        = timeout
      , _DtFetchReq'type'          = coerce type'
      }



fetchSet
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeSet)
  -> Bucket
  -> Key
  -> ( "basic_quorum"    := Bool
     , "include_context" := Bool
     , "n_val"           := Quorum
     , "notfound_ok"     := Bool
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := Bool
     , "timeout"         := Word32
     )
  -> m (Either RpbErrorResp [ByteString])
fetchSet handle type' bucket key params = runExceptT $ do
  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. L.type' of
    DtFetchResp'SET ->
      pure (response ^. L.value . L.setValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt
          DtFetchResp'SET)


fetchDataType
  :: MonadIO m
  => Handle
  -> BucketType ('Just ty)
  -> Bucket
  -> Key
  -> ( "basic_quorum"    := Bool
     , "include_context" := Bool
     , "n_val"           := Quorum
     , "notfound_ok"     := Bool
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := Bool
     , "timeout"         := Word32
     )
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn _) type' bucket key
    ( _ := basic_quorum
    , _ := include_context
    , _ := n_val
    , _ := notfound_ok
    , _ := pr
    , _ := r
    , _ := sloppy_quorum
    , _ := timeout
    ) =

  liftIO (exchange conn request)
 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = include_context
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = notfound_ok
      , _DtFetchReq'pr             = pr
      , _DtFetchReq'r              = r
      , _DtFetchReq'sloppyQuorum   = sloppy_quorum
      , _DtFetchReq'timeout        = timeout
      , _DtFetchReq'type'          = coerce type'
      }


-- TODO better updateCounter return type
-- Facts to encode:
--   * If key provided, riak doesn't return key
--   * If key not provided, riak returns random key
--   * If return_body, riak returns counter val
--   * If not return_body, riak doesn't return counter val
updateCounter
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Int64
  -> ( "dw"            := Quorum
     , "key"           := Key
     , "n_val"         := Quorum
     , "pw"            := Quorum
     , "return_body"   := Bool
     , "sloppy_quorum" := Bool
     , "timeout"       := Word32
     , "w"             := Quorum
     )
  -> m (Either RpbErrorResp DtUpdateResp)
updateCounter
    (Handle conn _) type' bucket incr
    ( _ := dw
    , _ := key
    , _ := n_val
    , _ := pw
    , _ := return_body
    , _ := sloppy_quorum
    , _ := timeout
    , _ := w
    ) = do
  liftIO (exchange conn request)
 where
  request :: DtUpdateReq
  request =
    DtUpdateReq
      { _DtUpdateReq'_unknownFields = []
      , _DtUpdateReq'bucket         = coerce bucket
      , _DtUpdateReq'context        = Nothing
      , _DtUpdateReq'dw             = dw
      , _DtUpdateReq'includeContext = Nothing
      , _DtUpdateReq'key            = coerce key
      , _DtUpdateReq'nVal           = n_val
      , _DtUpdateReq'op             = op
      , _DtUpdateReq'pw             = pw
      , _DtUpdateReq'returnBody     = return_body
      , _DtUpdateReq'sloppyQuorum   = sloppy_quorum
      , _DtUpdateReq'timeout        = timeout
      , _DtUpdateReq'type'          = coerce type'
      , _DtUpdateReq'w              = w
      }

  op :: DtOp
  op =
    DtOp
      { _DtOp'_unknownFields = []
      , _DtOp'counterOp      = Just (CounterOp (Just incr) [])
      , _DtOp'gsetOp         = Nothing
      , _DtOp'hllOp          = Nothing
      , _DtOp'mapOp          = Nothing
      , _DtOp'setOp          = Nothing
      }

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
