{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase,
             MagicHash, OverloadedLabels, OverloadedStrings, PatternSynonyms,
             RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Riak
  ( -- * Riak handle
    Handle
  , withHandle
  , VclockCache
  , refVclockCache
    -- * Key/value object operations
    -- ** Fetch object
  , fetchObject
    -- ** Store object
  , StoreObjectResp(..)
  , ObjectReturn(..)
  , SingObjectReturn
  , ObjectReturnTy
  , storeObject
    -- ** Delete object
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
    -- * Types
  , Bucket(..)
  , BucketType(..)
  , pattern BucketTypeDefault
  , Content(..)
  , ContentType(..)
  , DataType(..)
  , IfModified(..)
  , Key(..)
  , Quorum(..)
  , pattern QuorumAll
  , pattern QuorumDefault
  , pattern QuorumOne
  , pattern QuorumQuorum
  , Vclock(..)
  , Vtag(..)
    -- * Optional parameters
  , ParamBasicQuorum(..)
  , ParamDW(..)
  , ParamHead(..)
  , ParamIfModified(..)
  , ParamNotfoundOk(..)
  , ParamNVal(..)
  , ParamObjectReturn(..)
  , ParamPR(..)
  , ParamPW(..)
  , ParamR(..)
  , ParamReturnBody(..)
  , ParamReturnHead(..)
  , ParamSloppyQuorum(..)
  , ParamTimeout(..)
  , ParamW(..)
    -- * Temp
  , def
  ) where

import Control.Applicative
import Control.Monad              (guard, when)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString            (ByteString)
import Data.Coerce                (coerce)
import Data.Default.Class
import Data.Function              (fix)
import Data.HashMap.Strict        (HashMap)
import Data.Int
import Data.IORef
import Data.Kind                  (Type)
import Data.Text                  (Text)
import Data.Type.Bool             (If)
import Lens.Labels
import List.Transformer           (ListT)
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head, return, (.))
import UnliftIO.Exception         (throwIO)

import qualified Data.HashMap.Strict as HashMap
import qualified List.Transformer    as ListT

import           Proto.Riak
import qualified Riak.Internal            as Internal
import           Riak.Internal.Connection
import           Riak.Internal.Content
import           Riak.Internal.Panic
import           Riak.Internal.Request
import           Riak.Internal.Response
import           Riak.Internal.Types


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
-- API
--------------------------------------------------------------------------------

type FetchObjectResp (head :: Bool) (if_modified :: Bool) (a :: Type)
  = IfModifiedWrapper if_modified [(Content (If head () a))]

type family IfModifiedWrapper (if_modified :: Bool) (a :: Type) where
  IfModifiedWrapper 'True  a = IfModified a
  IfModifiedWrapper 'False a = a

fetchObject
  :: forall a head if_modified m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamHead a head
     , ParamIfModified if_modified
     , ParamNVal
     , ParamNotfoundOk
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp (Maybe (FetchObjectResp head if_modified a)))
fetchObject
    handle@(Handle conn cache) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , head
    , if_modified
    , ParamNVal n_val
    , ParamNotfoundOk notfound_ok
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , ParamTimeout timeout
    ) = liftIO . runExceptT $ do

  vclock :: Maybe Vclock <-
    case if_modified of
      ParamIfModified   -> lift (vclockCacheLookup cache type' bucket key)
      ParamNoIfModified -> pure Nothing

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = Just basic_quorum
        , _RpbGetReq'bucket         = coerce bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           =
            case head of
              ParamHead   -> Just True
              ParamNoHead -> Just False
        , _RpbGetReq'ifModified     =
            case if_modified of
              ParamIfModified   -> coerce vclock
              ParamNoIfModified -> Nothing
        , _RpbGetReq'key            = coerce key
        , _RpbGetReq'nVal           = n_val
        , _RpbGetReq'notfoundOk     = Just notfound_ok
        , _RpbGetReq'pr             = coerce (Just pr)
        , _RpbGetReq'r              = coerce (Just r)
        , _RpbGetReq'sloppyQuorum   = Just sloppy_quorum
        , _RpbGetReq'timeout        = timeout
        , _RpbGetReq'type'          = coerce (Just type')
        }

  response :: RpbGetResp <-
    ExceptT (Internal.fetchObject conn request)

  -- Only cache the vclock if we didn't received an "unmodified" response (which
  -- doesn't contain a vclock)
  case (if_modified, response ^. #maybe'unchanged) of
    (ParamIfModified, Just True) ->
      pure ()
    _ -> do
      cacheVclock handle type' bucket key (coerce (response ^. #maybe'vclock))

  lift (mkResponse response)

 where
  mkResponse
    :: RpbGetResp
    -> IO (Maybe (FetchObjectResp head if_modified a))
  mkResponse (RpbGetResp content _ unchanged _) =
    case if_modified of
      ParamIfModified ->
        case unchanged of
          Just True ->
            pure (Just Unmodified)

          _ ->
            (fmap.fmap) Modified contents

      ParamNoIfModified ->
        contents

   where
    contents :: IO (Maybe [Content (If head () a)])
    contents = runMaybeT $ do
      guard (not (null content))
      traverse (lift . parseContent (proxy# @_ @a) headAsBool) content
     where
      headAsBool :: SBool head
      headAsBool =
        case head of
          ParamHead   -> STrue
          ParamNoHead -> SFalse


data StoreObjectResp (a :: Type) (return :: ObjectReturn)
  = StoreObjectResp
      !Key
      !(ObjectReturnTy a return)

instance
  ( Show a
  , SingObjectReturn return
  ) => Show (StoreObjectResp a return) where
  show (StoreObjectResp key val) =
    case singObjectReturn @return of
      SObjectReturnNone ->
        "StoreObjectResp " ++ show key ++ " " ++ show val
      SObjectReturnHead ->
        "StoreObjectResp " ++ show key ++ " " ++ show val
      SObjectReturnBody ->
        "StoreObjectResp " ++ show key ++ " " ++ show val

-- deriving instance Show a => Show (StoreObjectResp a 'ObjectReturnNone)
-- deriving instance Show a => Show (StoreObjectResp a 'ObjectReturnHead)
-- deriving instance Show a => Show (StoreObjectResp a 'ObjectReturnBody)

type family ObjectReturnTy (a :: Type) (return :: ObjectReturn) where
  ObjectReturnTy _ 'ObjectReturnNone = ()
  ObjectReturnTy _ 'ObjectReturnHead = [Content ()]
  ObjectReturnTy a 'ObjectReturnBody = [Content a]

-- TODO storeObject: nicer input type than RpbContent
storeObject
  :: forall a m return.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamObjectReturn return
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> m (Either RpbErrorResp (StoreObjectResp a return))
storeObject handle type' bucket key content params =
  liftIO (runExceptT (storeObject_ handle type' bucket key content params))

storeObject_
  :: forall a return.
     IsContent a
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamObjectReturn return
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> ExceptT RpbErrorResp IO (StoreObjectResp a return)
storeObject_
    handle@(Handle conn cache) type' bucket key value
    ( ParamDW dw
    , ParamNVal n_val
    , ParamPW pw
    , return
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    , ParamW w
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
        , _RpbPutReq'content        =
            RpbContent
              { _RpbContent'_unknownFields = []
              , _RpbContent'charset = Nothing
              , _RpbContent'contentEncoding = coerce (contentEncoding value)
              , _RpbContent'contentType = coerce (Just (contentType (proxy# @_ @a)))
              , _RpbContent'deleted = Nothing
              , _RpbContent'indexes = [] -- TODO storeObject indexes
              , _RpbContent'lastMod = Nothing
              , _RpbContent'lastModUsecs = Nothing
              , _RpbContent'links = []
              , _RpbContent'ttl = Nothing -- TODO storeObject ttl
              , _RpbContent'usermeta = [] -- TODO storeObject usermeta
              , _RpbContent'value = contentEncode value
              , _RpbContent'vtag = Nothing
              }
        , _RpbPutReq'dw             = Just (coerce dw)
        , _RpbPutReq'ifNoneMatch    = Nothing
        , _RpbPutReq'ifNotModified  = Nothing
        , _RpbPutReq'key            = coerce key
        , _RpbPutReq'nVal           = n_val
        , _RpbPutReq'pw             = Just (coerce pw)
        , _RpbPutReq'returnBody     =
            case return of
              ParamObjectReturnNone -> Nothing
              ParamObjectReturnHead -> Nothing
              ParamObjectReturnBody -> Just True
        , _RpbPutReq'returnHead     =
            case return of
              ParamObjectReturnNone -> Nothing
              ParamObjectReturnHead -> Just True
              ParamObjectReturnBody -> Nothing
        , _RpbPutReq'sloppyQuorum   = Just sloppy_quorum
        , _RpbPutReq'timeout        = coerce timeout
        , _RpbPutReq'type'          = coerce (Just type')
        , _RpbPutReq'vclock         = coerce vclock
        , _RpbPutReq'w              = coerce (Just w)
        }

  response :: RpbPutResp <-
    ExceptT (Internal.storeObject conn request)

  let
    nonsense :: Text -> ExceptT RpbErrorResp IO void
    nonsense s =
      panic s
        ( ( "request",  request  )
        , ( "response", response )
        )

  theKey :: Key <-
    maybe
      (maybe
        (nonsense "missing key")
        pure
        (coerce (response ^. #maybe'key)))
      pure
      key

  -- Cache the vclock if asked for it with return_head or return_body.
  do
    let
      doCacheVclock :: ExceptT RpbErrorResp IO ()
      doCacheVclock =
        case response ^. #maybe'vclock of
          Nothing ->
            nonsense "missing vclock"

          Just theVclock ->
            cacheVclock handle type' bucket theKey (Just (coerce theVclock))

    () <-
      case return of
        ParamObjectReturnNone -> pure ()
        ParamObjectReturnHead -> doCacheVclock
        ParamObjectReturnBody -> doCacheVclock

    pure ()

  let
    theValue :: IO (ObjectReturnTy a return)
    theValue =
      case return of
        ParamObjectReturnNone ->
          pure ()

        ParamObjectReturnHead ->
          traverse (parseContent (proxy# @_ @a) STrue) (response ^. #content)

        ParamObjectReturnBody ->
          traverse (parseContent (proxy# @_ @a) SFalse) (response ^. #content)

  StoreObjectResp theKey <$> lift theValue


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
  -> ( ParamBasicQuorum
     , ParamNotfoundOk
     , ParamNVal
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp Int64)
fetchCounter
    (Handle conn _) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , ParamNotfoundOk notfound_ok
    , ParamNVal n_val
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , timeout
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
      , _DtFetchReq'basicQuorum    = Just basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Nothing
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = Just notfound_ok
      , _DtFetchReq'pr             = coerce (Just pr)
      , _DtFetchReq'r              = coerce (Just r)
      , _DtFetchReq'sloppyQuorum   = Just sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
      , _DtFetchReq'type'          = coerce type'
      }


fetchSet
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeSet)
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamIncludeContext
     , ParamNVal
     , ParamNotfoundOk -- TODO invert?
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp [ByteString])
fetchSet
    handle type' bucket key
    params@(_, ParamIncludeContext include_context, _, _, _, _, _, _) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. #type' of
    DtFetchResp'SET -> do
      -- Cache set context, if it was requested (it defaults to true)
      when include_context $
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
  -> ( ParamBasicQuorum
     , ParamIncludeContext
     , ParamNVal
     , ParamNotfoundOk
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn _) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , ParamIncludeContext include_context
    , ParamNVal n_val
    , ParamNotfoundOk notfound_ok
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    ) =

  liftIO (exchange conn request)
 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = Just basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Just include_context
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = Just notfound_ok
      , _DtFetchReq'pr             = coerce (Just pr)
      , _DtFetchReq'r              = coerce (Just r)
      , _DtFetchReq'sloppyQuorum   = Just sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
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
  -> Maybe Key
  -> Int64
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamReturnBody
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> m (Either RpbErrorResp DtUpdateResp)
updateCounter
    (Handle conn _) type' bucket key incr
    ( ParamDW dw
    , ParamNVal n_val
    , ParamPW pw
    , ParamReturnBody return_body
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    , ParamW w
    ) = do
  liftIO (exchange conn request)
 where
  request :: DtUpdateReq
  request =
    DtUpdateReq
      { _DtUpdateReq'_unknownFields = []
      , _DtUpdateReq'bucket         = coerce bucket
      , _DtUpdateReq'context        = Nothing
      , _DtUpdateReq'dw             = coerce (Just dw)
      , _DtUpdateReq'includeContext = Nothing
      , _DtUpdateReq'key            = coerce key
      , _DtUpdateReq'nVal           = n_val
      , _DtUpdateReq'op             = op
      , _DtUpdateReq'pw             = coerce (Just pw)
      , _DtUpdateReq'returnBody     = Just return_body
      , _DtUpdateReq'sloppyQuorum   = Just sloppy_quorum
      , _DtUpdateReq'timeout        = coerce timeout
      , _DtUpdateReq'type'          = coerce type'
      , _DtUpdateReq'w              = coerce (Just w)
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


-- TODO listBuckets param timeout
listBuckets
  :: MonadIO m
  => Handle
  -> BucketType ty
  -> ListT (ExceptT RpbErrorResp m) Bucket
listBuckets (Handle conn _) type' = do
  liftIO (send conn request)

  fix $ \loop -> do
    resp :: RpbListBucketsResp <-
      lift (ExceptT (liftIO (recv conn >>= parseResponse)))

    let
      buckets :: [Bucket]
      buckets =
        coerce (resp ^. #buckets)

    ListT.select buckets <|>
      if resp ^. #done
        then empty
        else loop
 where
  request :: RpbListBucketsReq
  request =
    RpbListBucketsReq
      { _RpbListBucketsReq'_unknownFields = []
      , _RpbListBucketsReq'stream = Just True
      , _RpbListBucketsReq'timeout = Nothing
      , _RpbListBucketsReq'type' = coerce (Just type')
      }

-- TODO listKeys param timeout
listKeys
  :: forall m ty.
     MonadIO m
  => Handle
  -> BucketType ty
  -> Bucket
  -> ListT (ExceptT RpbErrorResp m) Key
listKeys (Handle conn _) type' bucket = do
  liftIO (send conn request)

  fix $ \loop -> do
    resp :: RpbListKeysResp <-
      lift (ExceptT (liftIO (recv conn >>= parseResponse)))

    let
      keys :: [Key]
      keys =
        coerce (resp ^. #keys)

    ListT.select keys <|>
      if resp ^. #done
        then empty
        else loop
 where
  request :: RpbListKeysReq
  request =
    RpbListKeysReq
      { _RpbListKeysReq'_unknownFields = []
      , _RpbListKeysReq'bucket         = coerce bucket
      , _RpbListKeysReq'timeout        = Nothing
      , _RpbListKeysReq'type'          = coerce (Just type')
      }

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


parseContent
  :: forall a head.
     IsContent a
  => Proxy# a
  -> SBool head
  -> RpbContent
  -> IO (Content (If head () a))
parseContent _ head
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  theValue :: If head () a <-
    case head of
      STrue ->
        pure ()

      SFalse ->
        let
          value' :: ByteString
          value' =
            case coerce content_encoding of
              ContentEncodingNone -> value
              _                   -> undefined
              -- TODO handle unknown encoding
        in
          either throwIO pure (contentDecode value')

  pure $ Content
    theValue
    (coerce content_type)
    charset
    (coerce vtag)
    last_mod
    last_mod_usecs
    (map unRpbPair usermeta)
    (map unRpbPair indexes)
    deleted
    ttl


emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)


unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
unRpbPair (RpbPair k v _) =
  (k, v)
