{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase,
             MagicHash, OverloadedLabels, OverloadedStrings, PatternSynonyms,
             RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TupleSections, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances, ViewPatterns #-}

module Riak
  ( -- * Riak handle
    Handle
  , withHandle
  , VclockCache
  , refVclockCache
    -- * Key/value object operations
    -- ** Fetch object
  , fetchObject
  , fetchObjectHead
  , fetchObjectIfModified
  , fetchObjectIfModifiedHead
    -- ** Store object
  , storeObject
  , storeObjectHead
  , storeObjectBody
    -- ** Delete object
  , deleteObject
    -- * Data type operations
  , fetchCounter
  , fetchSet
  , fetchMap
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
  , BasicQuorum(..)
  , Bucket(..)
  , BucketType(..)
  , pattern BucketTypeDefault
  , Content(..)
  , ContentEncoding
  , pattern ContentEncodingNone
  , ContentType(..)
  , DataType(..)
  , DW(..)
  , Head(..)
  , IfModified(..)
  , Index(..)
  , Indexes(..)
  , IsContent(..)
  , Key(..)
  , Metadata(..)
  , Modified(..)
  , NotfoundOk(..)
  , Nval(..)
  , ParamObjectReturn(..) -- TODO rename ParamObjectReturn
  , PR(..)
  , PW(..)
  , Quorum(..)
  , pattern QuorumAll
  , pattern QuorumDefault
  , pattern QuorumOne
  , pattern QuorumQuorum
  , R(..)
  , ReturnBody(..)
  , SloppyQuorum(..)
  , Timeout(..)
  , TTL(..)
  , Vclock(..)
  , Vtag(..)
  , W(..)
    -- * Temp
  , def
  ) where

import Control.Applicative
import Control.Monad              (when)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.List.NonEmpty         (NonEmpty)
import Data.Coerce                (coerce)
import Data.Default.Class
import Data.Function              (fix)
import Data.HashMap.Strict        (HashMap)
import Data.Int
import Data.IORef
import Data.Kind                  (Type)
import Data.Maybe                 (fromMaybe)
import Data.Proxy                 (Proxy(Proxy))
import Data.Text                  (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Type.Bool             (If)
import Lens.Labels
import List.Transformer           (ListT)
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head, return, (.))
import Text.Read                  (readMaybe)
import UnliftIO.Exception         (throwIO)

import qualified Data.ByteString       as ByteString
import qualified Data.List.NonEmpty as List1
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.HashMap.Strict   as HashMap
import qualified List.Transformer      as ListT

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
  = IfModifiedWrapper if_modified [(Content (If head (Proxy a) a))]

type family IfModifiedWrapper (if_modified :: Bool) (a :: Type) where
  IfModifiedWrapper 'True  a = Modified a
  IfModifiedWrapper 'False a = a

fetchObject
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp [Content a])
fetchObject
    handle type' bucket key
    ( basic_quorum
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    ) =
  _fetchObject
    handle
    type'
    bucket
    key
    ( basic_quorum
    , NoHead
    , NoIfModified
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    )

fetchObjectHead
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp [Content (Proxy a)])
fetchObjectHead
    handle type' bucket key
    ( basic_quorum
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    ) =
  _fetchObject
    handle
    type'
    bucket
    key
    ( basic_quorum
    , Head
    , NoIfModified
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    )

fetchObjectIfModified
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp (Modified [Content a]))
fetchObjectIfModified
    handle type' bucket key
    ( basic_quorum
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    ) =
  _fetchObject
    handle
    type'
    bucket
    key
    ( basic_quorum
    , NoHead
    , IfModified
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    )

fetchObjectIfModifiedHead
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp (Modified [Content (Proxy a)]))
fetchObjectIfModifiedHead
    handle type' bucket key
    ( basic_quorum
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    ) =
  _fetchObject
    handle
    type'
    bucket
    key
    ( basic_quorum
    , Head
    , IfModified
    , n_val
    , notfound_ok
    , pr
    , r
    , sloppy_quorum
    , timeout
    )

-- TODO delete _fetchObject and inline its logic in the 4 variants?
_fetchObject
  :: forall a head if_modified m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , Head a head
     , IfModified if_modified
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp (FetchObjectResp head if_modified a))
_fetchObject
    handle@(Handle conn cache) type' bucket key
    ( BasicQuorum basic_quorum
    , head
    , if_modified
    , Nval n_val
    , NotfoundOk notfound_ok
    , PR pr
    , R r
    , SloppyQuorum sloppy_quorum
    , Timeout timeout
    ) = liftIO . runExceptT $ do

  vclock :: Maybe Vclock <-
    case if_modified of
      IfModified   -> lift (vclockCacheLookup cache type' bucket key)
      NoIfModified -> pure Nothing

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
              Head   -> Just True
              NoHead -> Just False
        , _RpbGetReq'ifModified     =
            case if_modified of
              IfModified   -> coerce vclock
              NoIfModified -> Nothing
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
    (IfModified, Just True) ->
      pure ()
    _ -> do
      cacheVclock handle type' bucket key (coerce (response ^. #maybe'vclock))

  lift (mkResponse response)

 where
  mkResponse
    :: RpbGetResp
    -> IO (FetchObjectResp head if_modified a)
  mkResponse (RpbGetResp content _ unchanged _) =
    case if_modified of
      IfModified ->
        case unchanged of
          Just True ->
            pure Unmodified

          _ ->
            Modified <$> contents

      NoIfModified ->
        contents

   where
    contents :: IO [Content (If head (Proxy a) a)]
    contents =
      traverse (parseContent @a proxy# headAsBool) content
     where
      headAsBool :: SBool head
      headAsBool =
        case head of
          Head   -> STrue
          NoHead -> SFalse


type family ObjectReturnTy (a :: Type) (return :: ObjectReturn) where
  ObjectReturnTy _ 'ObjectReturnNone = ()
  ObjectReturnTy a 'ObjectReturnHead = NonEmpty (Content (Proxy a))
  ObjectReturnTy a 'ObjectReturnBody = NonEmpty (Content a)

storeObject
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( DW
     , Indexes
     , Metadata
     , Nval
     , PW
     , SloppyQuorum
     , Timeout
     , TTL
     , W
     )
  -> m (Either RpbErrorResp Key)
storeObject handle type' bucket key content (a,b,c,d,e,f,g,h,i) = runExceptT $
  ExceptT (_storeObject handle type' bucket key content params) >>= \case
    (key', _) ->
      pure key'

 where
  params = (a,b,c,d,e,ParamObjectReturnNone,f,g,h,i)

storeObjectHead
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( DW
     , Indexes
     , Metadata
     , Nval
     , PW
     , SloppyQuorum
     , Timeout
     , TTL
     , W
     )
  -> m (Either RpbErrorResp (Key, NonEmpty (Content (Proxy a))))
storeObjectHead handle type' bucket key content (a,b,c,d,e,f,g,h,i) =
  _storeObject handle type' bucket key content params
 where
  params = (a,b,c,d,e,ParamObjectReturnHead,f,g,h,i)

storeObjectBody
  :: forall a m.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( DW
     , Indexes
     , Metadata
     , Nval
     , PW
     , SloppyQuorum
     , Timeout
     , TTL
     , W
     )
  -> m (Either RpbErrorResp (Key, NonEmpty (Content a)))
storeObjectBody handle type' bucket key content (a,b,c,d,e,f,g,h,i) =
  _storeObject handle type' bucket key content params
 where
  params = (a,b,c,d,e,ParamObjectReturnBody,f,g,h,i)

-- TODO inline _storeObject in 3 variants?
_storeObject
  :: forall a m return.
     (IsContent a, MonadIO m)
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> a
  -> ( DW
     , Indexes
     , Metadata
     , Nval
     , PW
     , ParamObjectReturn return
     , SloppyQuorum
     , Timeout
     , TTL
     , W
     )
  -> m (Either RpbErrorResp (Key, ObjectReturnTy a return))
_storeObject
    handle@(Handle conn cache) type' bucket key value
    ( DW dw
    , Indexes indexes
    , Metadata metadata
    , Nval n_val
    , PW pw
    , return
    , SloppyQuorum sloppy_quorum
    , timeout
    , TTL ttl
    , W w
    ) = liftIO . runExceptT $ do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe Vclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . vclockCacheLookup cache type' bucket)
      key

  let
    (content_type, charset, content_encoding, bytes) =
      contentEncode value

    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = Nothing
        , _RpbPutReq'bucket         = coerce bucket
        , _RpbPutReq'content        =
            RpbContent
              { _RpbContent'_unknownFields = []
              , _RpbContent'charset = coerce charset
              , _RpbContent'contentEncoding = coerce content_encoding
              , _RpbContent'contentType = coerce (Just content_type)
              , _RpbContent'deleted = Nothing
              , _RpbContent'indexes = map indexToRpbPair indexes
              , _RpbContent'lastMod = Nothing
              , _RpbContent'lastModUsecs = Nothing
              , _RpbContent'links = []
              , _RpbContent'ttl = ttl
              , _RpbContent'usermeta = map rpbPair metadata
              , _RpbContent'value = bytes
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
          traverse
            (parseContent @a proxy# STrue)
            (List1.fromList (response ^. #content))

        ParamObjectReturnBody ->
          traverse
            (parseContent @a proxy# SFalse)
            (List1.fromList (response ^. #content))

  (theKey ,) <$> lift theValue


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
  -> ( BasicQuorum
     , NotfoundOk
     , Nval
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp Int64)
fetchCounter
    (Handle conn _) type' bucket key
    ( BasicQuorum basic_quorum
    , NotfoundOk notfound_ok
    , Nval n_val
    , PR pr
    , R r
    , SloppyQuorum sloppy_quorum
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
  -> ( BasicQuorum
     , ParamIncludeContext
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp [ByteString])
fetchSet
    handle type' bucket key
    params@(_, ParamIncludeContext include_context, _, _, _, _, _, _) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. #type' of
    DtFetchResp'SET -> do
      when include_context $
        cacheVclock
          handle
          type'
          bucket
          key
          (coerce (response ^. #maybe'context))

      pure (response ^. #value . #setValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt DtFetchResp'SET)

fetchMap
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeMap)
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , ParamIncludeContext
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp [(ByteString, MapValue)])
fetchMap
    handle type' bucket key
    params@(_, ParamIncludeContext include_context, _, _, _, _, _, _) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. #type' of
    DtFetchResp'MAP -> do
      -- Cache set context, if it was requested (it defaults to true)
      when include_context $
        cacheVclock
          handle
          type'
          bucket
          key
          (coerce (response ^. #maybe'context))

      pure (map mapEntryToPair (response ^. #value . #mapValue))

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt DtFetchResp'MAP)


fetchDataType
  :: MonadIO m
  => Handle
  -> BucketType ('Just ty)
  -> Bucket
  -> Key
  -> ( BasicQuorum
     , ParamIncludeContext
     , Nval
     , NotfoundOk
     , PR
     , R
     , SloppyQuorum
     , Timeout
     )
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn _) type' bucket key
    ( BasicQuorum basic_quorum
    , ParamIncludeContext include_context
    , Nval n_val
    , NotfoundOk notfound_ok
    , PR pr
    , R r
    , SloppyQuorum sloppy_quorum
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
  -> ( DW
     , Nval
     , PW
     , ReturnBody
     , SloppyQuorum
     , Timeout
     , W
     )
  -> m (Either RpbErrorResp DtUpdateResp)
updateCounter
    (Handle conn _) type' bucket key incr
    ( DW dw
    , Nval n_val
    , PW pw
    , ReturnBody return_body
    , SloppyQuorum sloppy_quorum
    , timeout
    , W w
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
  -> IO (Content (If head (Proxy a) a))
parseContent _ head
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  theValue :: If head (Proxy a) a <-
    case head of
      STrue ->
        pure Proxy

      SFalse ->
        either
          throwIO
          pure
          (contentDecode
            (coerce content_type)
            (coerce charset)
            (coerce content_encoding )
            value)

  let
    theLastMod :: Maybe NominalDiffTime
    theLastMod = do
      secs  <- last_mod
      usecs <- last_mod_usecs <|> pure 0
      let usecs_d = realToFrac usecs / 1000000 :: Double
      pure (fromIntegral secs + realToFrac usecs_d)

  pure $ Content
    theValue
    (coerce vtag)
    (posixSecondsToUTCTime <$> theLastMod)
    (coerce (map unRpbPair usermeta))
    (coerce (map rpbPairToIndex indexes))
    (fromMaybe False deleted)
    (coerce ttl)


emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)


indexToRpbPair :: Index -> RpbPair
indexToRpbPair = \case
  IndexInt k v ->
    RpbPair k (Just (Latin1.pack (show v))) [] -- TODO more efficient show

  IndexBin k v ->
    RpbPair k (Just v) []


mapEntryToPair :: MapEntry -> (ByteString, MapValue)
mapEntryToPair entry =
  (k ,) $
    case ty of
      MapField'COUNTER ->
        MapValueCounter (entry ^. #counterValue)

      MapField'FLAG ->
        MapValueFlag (entry ^. #flagValue)

      MapField'MAP ->
        MapValueMap (map mapEntryToPair (entry ^. #mapValue))

      MapField'REGISTER ->
        MapValueRegister (entry ^. #registerValue)

      MapField'SET ->
        MapValueSet (entry ^. #setValue)

 where
  MapField k ty _ =
    entry ^. #field


rpbPairToIndex :: RpbPair -> Index
rpbPairToIndex = \case
  RpbPair (ByteString.stripSuffix "_bin" -> Just k) (Just v) _ ->
    IndexBin k v

  RpbPair
      (ByteString.stripSuffix "_int" -> Just k)
      (Just (readMaybe . Latin1.unpack -> Just v)) _ ->
    IndexInt k v -- TODO better read

  -- TODO what to do if index value is empty...?
  _ ->
    undefined


rpbPair :: (ByteString, Maybe ByteString) -> RpbPair
rpbPair (k, v) =
  RpbPair k v []


unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
unRpbPair (RpbPair k v _) =
  (k, v)
