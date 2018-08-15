{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving, InstanceSigs,
             KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses,
             OverloadedLabels, RankNTypes, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Riak
  ( -- * Riak handle
    Handle
  , withHandle
  , VclockCache
  , refVclockCache
    -- * Key/value object operations
  , FetchObjectResp(..)
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
  , Content(..)
  , ContentType(..)
  , DataType(..)
  , Key(..)
  , Vclock(..)
  , Vtag(..)
    -- * Temp
  , def
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Coerce                (coerce)
import Data.Hashable              (Hashable)
import Data.HashMap.Strict        (HashMap)
import Data.Int
import Data.IORef
import Data.Maybe                 (isJust)
import Data.Text.Encoding         (decodeUtf8)
import Data.Word
import GHC.Exts                   (IsString)
import Lens.Family2.Unchecked     (lens)
import Lens.Labels
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head, (.))
import UnliftIO.Exception         (Exception, throwIO)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text              as Text

import           Proto.Riak
import qualified Riak.Internal            as Internal
import           Riak.Internal.Connection
import           Riak.Internal.Param
import           Riak.Internal.Request
import           Riak.Internal.Response


--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Connection
      !VclockCache

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> VclockCache
  -> (Handle -> m a)
  -> m a
withHandle host port cache f = do
  withConnection host port (\conn -> f (Handle conn cache))


data VclockCache
  = VclockCache
      { vclockCacheLookup ::
          !(forall ty. BucketType ty -> Bucket -> Key -> IO (Maybe Vclock))
      , vclockCacheInsert ::
          !(forall ty. BucketType ty -> Bucket -> Key -> Vclock -> IO ())
      , vclockCacheDelete ::
          !(forall ty. BucketType ty -> Bucket -> Key -> IO ())
      }

-- | Make a 'VclockCache' backed by an 'IORef' + 'HashMap' that never purges
-- entries. TODO Smarter cache invalidation (controllable timeout).
refVclockCache :: MonadIO m => m VclockCache
refVclockCache = liftIO $ do
  -- TODO strict (type, bucket, key)
  cacheRef :: IORef (HashMap (SomeBucketType, Bucket, Key) Vclock) <-
    liftIO (newIORef mempty)

  pure VclockCache
    { vclockCacheLookup =
        \type' bucket key -> do
          HashMap.lookup (coerce (unBucketType type'), bucket, key) <$>
            readIORef cacheRef

    , vclockCacheInsert =
        \type' bucket key vclock ->
          modifyIORef' cacheRef
            (HashMap.insert (coerce (unBucketType type'), bucket, key) vclock)

    , vclockCacheDelete =
        \type' bucket key ->
          modifyIORef cacheRef
            (HashMap.delete (coerce (unBucketType type'), bucket, key))
    }


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A Riak bucket type, tagged with the data type it contains.
newtype BucketType (ty :: Maybe DataType)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)


-- | A Riak bucket.
newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Bucket where
  show :: Bucket -> String
  show =
    Text.unpack . decodeUtf8 . unBucket


data Content
  = Content
      !ByteString                       -- Value
      !(Maybe ContentType)
      !(Maybe ByteString)               -- Charset
      !(Maybe ByteString)               -- Content encoding
      !(Maybe Vtag)
      !(Maybe Word32)                   -- Last modified
      !(Maybe Word32)                   -- Last modified usecs
      ![(ByteString, Maybe ByteString)] -- User meta
      ![(ByteString, Maybe ByteString)] -- Indexes
      !(Maybe Bool)                     -- Deleted
      !(Maybe Word32)                   -- TTL
  deriving (Show)

instance (HasLens' f Content x a, a ~ b) => HasLens f Content Content x a b where lensOf = lensOf'

instance Functor f => HasLens' f Content "value"           ByteString                       where lensOf' _ = lens (\(Content x _ _ _ _ _ _ _ _ _ _) -> x) (\(Content _ b c d e f g h i j k) x -> Content x b c d e f g h i j k)
instance Functor f => HasLens' f Content "contentType"     (Maybe ContentType)              where lensOf' _ = lens (\(Content _ x _ _ _ _ _ _ _ _ _) -> x) (\(Content a _ c d e f g h i j k) x -> Content a x c d e f g h i j k)
instance Functor f => HasLens' f Content "charset"         (Maybe ByteString)               where lensOf' _ = lens (\(Content _ _ x _ _ _ _ _ _ _ _) -> x) (\(Content a b _ d e f g h i j k) x -> Content a b x d e f g h i j k)
instance Functor f => HasLens' f Content "contentEncoding" (Maybe ByteString)               where lensOf' _ = lens (\(Content _ _ _ x _ _ _ _ _ _ _) -> x) (\(Content a b c _ e f g h i j k) x -> Content a b c x e f g h i j k)
instance Functor f => HasLens' f Content "vtag"            (Maybe Vtag)                     where lensOf' _ = lens (\(Content _ _ _ _ x _ _ _ _ _ _) -> x) (\(Content a b c d _ f g h i j k) x -> Content a b c d x f g h i j k)
instance Functor f => HasLens' f Content "lastMod"         (Maybe Word32)                   where lensOf' _ = lens (\(Content _ _ _ _ _ x _ _ _ _ _) -> x) (\(Content a b c d e _ g h i j k) x -> Content a b c d e x g h i j k)
instance Functor f => HasLens' f Content "lastModUsecs"    (Maybe Word32)                   where lensOf' _ = lens (\(Content _ _ _ _ _ _ x _ _ _ _) -> x) (\(Content a b c d e f _ h i j k) x -> Content a b c d e f x h i j k)
instance Functor f => HasLens' f Content "usermeta"        [(ByteString, Maybe ByteString)] where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ x _ _ _) -> x) (\(Content a b c d e f g _ i j k) x -> Content a b c d e f g x i j k)
instance Functor f => HasLens' f Content "indexes"         [(ByteString, Maybe ByteString)] where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ x _ _) -> x) (\(Content a b c d e f g h _ j k) x -> Content a b c d e f g h x j k)
instance Functor f => HasLens' f Content "deleted"         (Maybe Bool)                     where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ _ x _) -> x) (\(Content a b c d e f g h i _ k) x -> Content a b c d e f g h i x k)
instance Functor f => HasLens' f Content "ttl"             (Maybe Word32)                   where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ _ _ x) -> x) (\(Content a b c d e f g h i j _) x -> Content a b c d e f g h i j x)


newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)


data DataType
  = DataTypeCounter
  | DataTypeMap
  | DataTypeSet
  -- TODO hll, gset


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


-- | A Riak key.
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

instance Show Vclock where
  show :: Vclock -> String
  show =
    show . Base64.encode . unVclock


newtype Vtag
  = Vtag { unVtag :: ByteString }
  deriving (Show)


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

data FetchObjectResp
  = FetchObjectResp
      ![Content]      -- Content
      !(Maybe Vclock) -- Vclock
      !(Maybe Bool)   -- Unchanged
  deriving (Show)

instance (HasLens' f FetchObjectResp x a, a ~ b) => HasLens f FetchObjectResp FetchObjectResp x a b where lensOf = lensOf'

instance Functor f => HasLens' f FetchObjectResp "content"   [Content]      where lensOf' _ = lens (\(FetchObjectResp a _ _) -> a) (\(FetchObjectResp _ b c) a -> FetchObjectResp a b c)
instance Functor f => HasLens' f FetchObjectResp "vclock"    (Maybe Vclock) where lensOf' _ = lens (\(FetchObjectResp _ b _) -> b) (\(FetchObjectResp a _ c) b -> FetchObjectResp a b c)
instance Functor f => HasLens' f FetchObjectResp "unchanged" (Maybe Bool)   where lensOf' _ = lens (\(FetchObjectResp _ _ c) -> c) (\(FetchObjectResp a b _) c -> FetchObjectResp a b c)


-- TODO fetchObject: better return type
fetchObject
  :: MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( "basic_quorum"  := ()
     , "head"          := ()
     , "if_modified"   := ()
     , "n_val"         := Quorum
     , "notfound_ok"   := ()
     , "pr"            := Quorum
     , "r"             := Quorum
     , "sloppy_quorum" := ()
     , "timeout"       := Word32
     )
  -> m (Either RpbErrorResp (Maybe FetchObjectResp))
fetchObject
    handle@(Handle conn cache) type' bucket key
    ( _ := basic_quorum
    , _ := head
    , _ := if_modified
    , _ := n_val
    , _ := notfound_ok
    , _ := pr
    , _ := r
    , _ := sloppy_quorum
    , _ := timeout
    ) = liftIO . runExceptT $ do

  vclock :: Maybe Vclock <-
    if isJust if_modified
      then lift (vclockCacheLookup cache type' bucket key)
      else pure Nothing

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = True <$ basic_quorum
        , _RpbGetReq'bucket         = coerce bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           = True <$ head
        , _RpbGetReq'ifModified     = coerce vclock
        , _RpbGetReq'key            = coerce key
        , _RpbGetReq'nVal           = n_val
        , _RpbGetReq'notfoundOk     = True <$ notfound_ok
        , _RpbGetReq'pr             = pr
        , _RpbGetReq'r              = r
        , _RpbGetReq'sloppyQuorum   = True <$ sloppy_quorum
        , _RpbGetReq'timeout        = timeout
        , _RpbGetReq'type'          = coerce (Just type')
        }

  response :: RpbGetResp <-
    ExceptT (Internal.fetchObject conn request)

  -- TODO; this logic assumes if_modified is not set. If it is, and the object
  -- is up to date, then we will have content = [], vclock = Nothing,
  -- unchanged = Just True.
  cacheVclock handle type' bucket key (coerce (response ^. #maybe'vclock))

  case response ^. #content of
    [] ->
      pure Nothing
    _ ->
      pure (Just (mkResponse response))

 where
  mkResponse :: RpbGetResp -> FetchObjectResp
  mkResponse (RpbGetResp content vclock unchanged _) =
    FetchObjectResp (map mkContent content) (coerce vclock) unchanged

  mkContent :: RpbContent -> Content
  mkContent
      (RpbContent value content_type charset content_encoding vtag _ last_mod
                  last_mod_usecs usermeta indexes deleted ttl _) =
    Content
      value
      (coerce content_type)
      charset
      content_encoding
      (coerce vtag)
      last_mod
      last_mod_usecs
      (map unRpbPair usermeta)
      (map unRpbPair indexes)
      deleted
      ttl


-- TODO storeObject: nicer input type than RpbContent
-- TODO storeObject: better return type
storeObject
  :: MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> RpbContent
  -> ( "dw"              := Quorum
     , "key"             := Key
     , "n_val"           := Quorum
     , "pw"              := Quorum
     , "return_body"     := ()
     , "return_head"     := Bool
     , "sloppy_quorum"   := ()
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
  -> ( "dw"              := Quorum
     , "key"             := Key
     , "n_val"           := Quorum
     , "pw"              := Quorum
     , "return_body"     := ()
     , "return_head"     := Bool -- TODO figure out what this defaults to
     , "sloppy_quorum"   := ()
     , "timeout"         := Word32
     , "w"               := Quorum
     )
  -> ExceptT RpbErrorResp IO RpbPutResp
storeObject_
    (Handle conn cache) type' bucket content
    ( _ := dw
    , _ := key
    , _ := n_val
    , _ := pw
    , _ := return_body
    , _ := return_head
    , _ := sloppy_quorum
    , _ := timeout
    , _ := w
    ) = do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe Vclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . vclockCacheLookup cache type' bucket)
      key

  let
    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = Nothing
        , _RpbPutReq'bucket         = coerce bucket
        , _RpbPutReq'content        = content
        , _RpbPutReq'dw             = dw
        , _RpbPutReq'ifNoneMatch    = Nothing
        , _RpbPutReq'ifNotModified  = Nothing
        , _RpbPutReq'key            = coerce key
        , _RpbPutReq'nVal           = n_val
        , _RpbPutReq'pw             = pw
        , _RpbPutReq'returnBody     = True <$ return_body
        , _RpbPutReq'returnHead     = return_head
        , _RpbPutReq'sloppyQuorum   = True <$ sloppy_quorum
        , _RpbPutReq'timeout        = timeout
        , _RpbPutReq'type'          = coerce (Just type')
        , _RpbPutReq'vclock         = coerce vclock
        , _RpbPutReq'w              = w
        }

  ExceptT (Internal.storeObject conn request)


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
  -> ( "basic_quorum"    := ()
     , "n_val"           := Quorum
     , "notfound_ok"     := ()
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := ()
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

  case response ^. #type' of
    DtFetchResp'COUNTER ->
      pure (response ^. #value . #counterValue)

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
      , _DtFetchReq'basicQuorum    = True <$ basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Nothing
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = True <$ notfound_ok
      , _DtFetchReq'pr             = pr
      , _DtFetchReq'r              = r
      , _DtFetchReq'sloppyQuorum   = True <$ sloppy_quorum
      , _DtFetchReq'timeout        = timeout
      , _DtFetchReq'type'          = coerce type'
      }


fetchSet
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeSet)
  -> Bucket
  -> Key
  -> ( "basic_quorum"    := ()
     , "include_context" := Bool -- TODO rename to not_include_context?
     , "n_val"           := Quorum
     , "notfound_ok"     := ()
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := ()
     , "timeout"         := Word32
     )
  -> m (Either RpbErrorResp [ByteString])
fetchSet
    handle type' bucket key
    params@(_, _ := include_context, _, _, _, _, _, _) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. #type' of
    DtFetchResp'SET -> do
      -- Cache set context, if it was requested (it defaults to true)
      when (include_context /= Just False) $
        let
          vclock :: Maybe Vclock
          vclock = coerce (response ^. #maybe'context)
        in
          cacheVclock handle type' bucket key vclock

      pure (response ^. #value . #setValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt DtFetchResp'SET)


fetchDataType
  :: MonadIO m
  => Handle
  -> BucketType ('Just ty)
  -> Bucket
  -> Key
  -> ( "basic_quorum"    := ()
     , "include_context" := Bool
     , "n_val"           := Quorum
     , "notfound_ok"     := ()
     , "pr"              := Quorum
     , "r"               := Quorum
     , "sloppy_quorum"   := ()
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
      , _DtFetchReq'basicQuorum    = True <$ basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = include_context
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = True <$ notfound_ok
      , _DtFetchReq'pr             = pr
      , _DtFetchReq'r              = r
      , _DtFetchReq'sloppyQuorum   = True <$ sloppy_quorum
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
     , "return_body"   := ()
     , "sloppy_quorum" := ()
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
      , _DtUpdateReq'returnBody     = True <$ return_body
      , _DtUpdateReq'sloppyQuorum   = True <$ sloppy_quorum
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

      if resp ^. #done
        then pure (resp ^. #keys)
        else ((resp ^. #keys) ++) <$> loop

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

      if resp ^. #done
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


--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

-- | Given a fetched vclock, update the cache (if present) or delete it from the
-- cache (if missing).
cacheVclock
  :: MonadIO m
  => Handle
  -> BucketType ty
  -> Bucket
  -> Key
  -> Maybe Vclock
  -> m ()
cacheVclock (Handle _ cache) type' bucket key = liftIO .
  maybe
    (vclockCacheDelete cache type' bucket key)
    (vclockCacheInsert cache type' bucket key)


emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)


unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
unRpbPair (RpbPair k v _) =
  (k, v)
