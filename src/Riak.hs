{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, FlexibleInstances,
             LambdaCase, MagicHash, NoImplicitPrelude, OverloadedLabels,
             OverloadedStrings, PatternSynonyms, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TupleSections,
             TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances, ViewPatterns #-}

module Riak
  ( -- * Handle
    createRiakHandle
    -- * Object operations
    -- ** Fetch object
  , fetchRiakObject
  , fetchRiakObjectHead
  , fetchRiakObjectIfModified
  , fetchRiakObjectIfModifiedHead
    -- ** Store object
  , storeRiakObject
  , storeRiakObjectHead
  , storeRiakObjectBody
  , storeNewRiakObject
  , storeNewRiakObject2
  , storeNewRiakObjectHead
  , storeNewRiakObjectBody
    -- ** Delete object
  , deleteRiakObject
    -- * Data type operations
    -- ** Counter
  , fetchRiakCounter
  , updateRiakCounter
  , updateNewRiakCounter
    -- ** Grow-only set
  , fetchRiakGrowOnlySet
    -- ** HyperLogLog
  , fetchRiakHyperLogLog
    -- ** Set
  , fetchRiakSet
  , updateRiakSet
  , updateNewRiakSet
  , riakSetAddOp
  , riakSetRemoveOp
    -- ** Map
  , fetchRiakMap
  , riakCounterField
  , riakFlagField
  , riakMapField
  , riakRegisterField
  , riakSetField
  -- , allowExtraKeys
    -- * Bucket operations
  , getRiakBucketTypeProps
  , setRiakBucketTypeProps
  , getRiakBucketProps
  , setRiakBucketProps
  , resetRiakBucketProps
  , listRiakBuckets
  , listRiakKeys
    -- * MapReduce
  , riakMapReduce
    -- * Secondary indexes (2i)
    -- * Search 2.0
    -- ** Schema
  , getRiakSchema
  , putRiakSchema
    -- ** Index
  , getRiakIndex
  , getRiakIndexes
  , putRiakIndex
  , deleteRiakIndex
    -- * Server info
  , pingRiak
  , getRiakServerInfo
    -- * Types
  , Charset(..)
  , ContentEncoding(..)
  , ContentType(..)
  , FetchRiakCrdtParams
  , FetchRiakObjectParams
  , IsRiakContent(..)
  , IsRiakMap(..)
  , IsRiakRegister(..)
  , IsRiakSet
  , Modified(..)
  , RiakBucket(..)
  , RiakBucketType(..)
  , pattern DefaultRiakBucketType
  , RiakContent(..)
  , RiakCrdtError(..)
  , RiakCrdtTy(..)
  , RiakError(..)
  , RiakHandle
  , RiakIndexName(..)
  , RiakKey(..)
  , RiakLocation(..)
  , RiakMapEntries(..)
  , RiakMapFieldParser
  , RiakMapParseError(..)
  , RiakMetadata(..)
  , RiakNamespace(..)
  , RiakQuorum(..)
  , pattern RiakQuorumAll
  , pattern RiakQuorumQuorum
  , RiakSchemaName(..)
  , pattern DefaultRiakSchemaName
  , RiakSecondaryIndex(..)
  , RiakSetOp
  , RiakVtag(..)
  , StoreRiakObjectParams
  , TTL(..)
  , UpdateRiakCrdtParams
    -- * Re-exports
  , def
    -- * Documentation
    -- $documentation
  ) where

import Data.Default.Class    (def)
import Data.Time             (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Labels
import Network.Socket        (HostName, PortNumber)

import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.List.NonEmpty    as List1
import qualified List.Transformer      as ListT

import           Proto.Riak               hiding (SetOp)
import qualified Proto.Riak               as Proto
import qualified Riak.Internal            as Internal
import           Riak.Internal.Cache
import           Riak.Internal.Content
import           Riak.Internal.Crdts
import           Riak.Internal.Manager
import           Riak.Internal.Panic
import           Riak.Internal.Params
import           Riak.Internal.Prelude
import           Riak.Internal.Response
import           Riak.Internal.Types


-- TODO _ variants that don't decode replies
-- TODO rename "content" to "object"

--------------------------------------------------------------------------------
-- RiakHandle
--------------------------------------------------------------------------------

-- | A thread-safe handle to Riak.
--
-- TODO: RiakHandle improvement: cluster
data RiakHandle
  = RiakHandle
      !RiakManager
      !Cache


-- TODO createRiakHandle configurable maximum number of sockets
-- TODO createRiakHandle close connections after
createRiakHandle
  :: MonadIO m
  => HostName -- ^
  -> PortNumber -- ^
  -> m RiakHandle
createRiakHandle host port = do
  cache :: Cache <-
    liftIO newCache

  manager :: RiakManager <-
    liftIO (createRiakManager host port)

  pure (RiakHandle manager cache)


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Fetch an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'storeRiakObject'.
fetchRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^ Location of the object.
  -> FetchRiakObjectParams -- ^ Optional parameters.
  -> m (Either RiakError [RiakContent a])
fetchRiakObject handle loc (FetchRiakObjectParams a b c d e f g) = _fetchRiakObject handle loc a NoHead NoIfModified b c d e f g

-- | Fetch an object's metadata.
fetchRiakObjectHead
    :: forall a m.
       (IsRiakContent a, MonadIO m)
    => RiakHandle -- ^ Riak handle.
    -> RiakLocation 'Nothing -- ^
    -> FetchRiakObjectParams
    -> m (Either RiakError [RiakContent (Proxy a)])
fetchRiakObjectHead handle loc (FetchRiakObjectParams a b c d e f g) = _fetchRiakObject handle loc a Head NoIfModified b c d e f g

-- | Fetch an object if it has been modified since the last fetch.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'storeRiakObject'.
fetchRiakObjectIfModified
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^
  -> FetchRiakObjectParams -- ^
  -> m (Either RiakError (Modified [RiakContent a]))
fetchRiakObjectIfModified handle loc (FetchRiakObjectParams a b c d e f g) = _fetchRiakObject handle loc a NoHead IfModified b c d e f g

-- | Fetch an object's metadata if it has been modified since the last fetch.
fetchRiakObjectIfModifiedHead
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^
  -> FetchRiakObjectParams -- ^
  -> m (Either RiakError (Modified [RiakContent (Proxy a)]))
fetchRiakObjectIfModifiedHead handle loc (FetchRiakObjectParams a b c d e f g) = _fetchRiakObject handle loc a Head IfModified b c d e f g

type FetchRiakObjectResp (head :: Bool) (if_modified :: Bool) (a :: Type)
  = IfModifiedWrapper if_modified [(RiakContent (If head (Proxy a) a))]

type family IfModifiedWrapper (if_modified :: Bool) (a :: Type) where
  IfModifiedWrapper 'True  a = Modified a
  IfModifiedWrapper 'False a = a

-- TODO delete _fetchRiakObject and inline its logic in the 4 variants?
_fetchRiakObject
  :: forall a head if_modified m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle
  -> RiakLocation 'Nothing
  -> BasicQuorum
  -> Head a head
  -> IfModified if_modified
  -> N
  -> NotfoundOk
  -> PR
  -> R
  -> SloppyQuorum
  -> Timeout
  -> m (Either RiakError (FetchRiakObjectResp head if_modified a))
_fetchRiakObject
    handle@(RiakHandle manager cache)
    loc@(RiakLocation (RiakNamespace type' bucket) key)
    basic_quorum head if_modified n notfound_ok pr r sloppy_quorum timeout =
    liftIO . runExceptT $ do

  vclock :: Maybe RiakVclock <-
    case if_modified of
      IfModified   -> lift (cacheLookup cache loc)
      NoIfModified -> pure Nothing

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = unBasicQuorum basic_quorum
        , _RpbGetReq'bucket         = unRiakBucket bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           =
            case head of
              Head   -> Just True
              NoHead -> Just False
        , _RpbGetReq'ifModified     =
            case if_modified of
              IfModified   -> coerce vclock
              NoIfModified -> Nothing
        , _RpbGetReq'key            = unRiakKey key
        , _RpbGetReq'nVal           = coerce n
        , _RpbGetReq'notfoundOk     = unNotfoundOk notfound_ok
        , _RpbGetReq'pr             = coerce pr
        , _RpbGetReq'r              = coerce r
        , _RpbGetReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
        , _RpbGetReq'timeout        = unTimeout timeout
        , _RpbGetReq'type'          = Just (unRiakBucketType type')
        }

  response :: RpbGetResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.fetchRiakObject conn request))

  -- Only cache the vclock if we didn't received an "unmodified" response (which
  -- doesn't contain a vclock)
  case (if_modified, response ^. #maybe'unchanged) of
    (IfModified, Just True) ->
      pure ()
    _ -> do
      cacheVclock handle loc (coerce (response ^. #maybe'vclock))

  lift (mkResponse response)

 where
  mkResponse
    :: RpbGetResp
    -> IO (FetchRiakObjectResp head if_modified a)
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
    contents :: IO [RiakContent (If head (Proxy a) a)]
    contents =
      traverse (parseContent @a proxy# loc headAsBool) content
     where
      headAsBool :: SBool head
      headAsBool =
        case head of
          Head   -> STrue
          NoHead -> SFalse


type family ObjectReturnTy (a :: Type) (return :: ObjectReturn) where
  ObjectReturnTy _ 'ObjectReturnNone = RiakKey
  ObjectReturnTy a 'ObjectReturnHead = NonEmpty (RiakContent (Proxy a))
  ObjectReturnTy a 'ObjectReturnBody = NonEmpty (RiakContent a)

-- | Store an object.
storeRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError ())
storeRiakObject
    handle (RiakLocation namespace key) content (StoreRiakObjectParams a b c d e f g h) =
  fmap (() <$)
    (_storeRiakObject handle namespace (Just key) content a b c d e
      ParamObjectReturnNone f g h)

-- | Store an object and return its metadata.
storeRiakObjectHead
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError (NonEmpty (RiakContent (Proxy a))))
storeRiakObjectHead
    handle (RiakLocation namespace key) content (StoreRiakObjectParams a b c d e f g h) =
  _storeRiakObject handle namespace (Just key) content a b c d e
    ParamObjectReturnHead f g h

-- | Store an object and return it.
storeRiakObjectBody
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError (NonEmpty (RiakContent a)))
storeRiakObjectBody
    handle (RiakLocation namespace key) content (StoreRiakObjectParams a b c d e f g h) =
  _storeRiakObject handle namespace (Just key) content a b c d e
    ParamObjectReturnBody f g h

-- | Store a new object and return its randomly-generated key.
storeNewRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError RiakKey)
storeNewRiakObject
    handle namespace content (StoreRiakObjectParams a b c d e f g h) =
  _storeRiakObject handle namespace Nothing content a b c d e
    ParamObjectReturnNone f g h

-- | Store a new object and return its randomly-generated key.
storeNewRiakObject2
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError RiakKey)
storeNewRiakObject2
    handle namespace content (StoreRiakObjectParams a b c d e f g h) =
  _storeRiakObject handle namespace Nothing content a b c d e
    ParamObjectReturnNone f g h

-- | Store an new object and return its metadata.
storeNewRiakObjectHead
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError (RiakContent (Proxy a)))
storeNewRiakObjectHead
    handle namespace content (StoreRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head $
    _storeRiakObject handle namespace Nothing content a b c d e
      ParamObjectReturnHead f g h

-- | Store an new object and return it.
storeNewRiakObjectBody
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace 'Nothing -- ^
  -> a -- ^
  -> StoreRiakObjectParams -- ^
  -> m (Either RiakError (RiakContent a))
storeNewRiakObjectBody
    handle namespace content (StoreRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head $
    _storeRiakObject handle namespace Nothing content a b c d e
      ParamObjectReturnBody f g h

_storeRiakObject
  :: forall a m return.
     (IsRiakContent a, MonadIO m)
  => RiakHandle
  -> RiakNamespace 'Nothing
  -> Maybe RiakKey
  -> a
  -> DW
  -> [RiakSecondaryIndex]
  -> RiakMetadata
  -> N
  -> PW
  -> ParamObjectReturn return
  -> SloppyQuorum
  -> Timeout
  -> W
  -> m (Either RiakError (ObjectReturnTy a return))
_storeRiakObject
    handle@(RiakHandle manager cache) namespace@(RiakNamespace type' bucket) key
    value dw indexes metadata n pw return sloppy_quorum timeout
    w = liftIO . runExceptT $ do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe RiakVclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . cacheLookup cache . RiakLocation namespace)
      key

  let
    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = Nothing
        , _RpbPutReq'bucket         = unRiakBucket bucket
        , _RpbPutReq'content        =
            RpbContent
              { _RpbContent'_unknownFields  = []
              , _RpbContent'charset         = coerce (riakCharset value)
              , _RpbContent'contentEncoding = coerce (riakContentEncoding value)
              , _RpbContent'contentType     = Just (unContentType (riakContentType value))
              , _RpbContent'deleted         = Nothing
              , _RpbContent'indexes         = map indexToRpbPair (coerce indexes)
              , _RpbContent'lastMod         = Nothing
              , _RpbContent'lastModUsecs    = Nothing
              , _RpbContent'links           = []
              , _RpbContent'ttl             = Nothing
              , _RpbContent'usermeta        = map rpbPair (coerce metadata)
              , _RpbContent'value           = encodeRiakContent value
              , _RpbContent'vtag            = Nothing
              }
        , _RpbPutReq'dw             = coerce dw
        , _RpbPutReq'ifNoneMatch    = Nothing
        , _RpbPutReq'ifNotModified  = Nothing
        , _RpbPutReq'key            = coerce key
        , _RpbPutReq'nVal           = unN n
        , _RpbPutReq'pw             = coerce pw
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
        , _RpbPutReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
        , _RpbPutReq'timeout        = unTimeout timeout
        , _RpbPutReq'type'          = Just (unRiakBucketType type')
        , _RpbPutReq'vclock         = coerce vclock
        , _RpbPutReq'w              = coerce w
        }

  response :: RpbPutResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.storeRiakObject conn request))

  let
    nonsense :: Text -> ExceptT RiakError IO void
    nonsense s =
      panic s
        ( ( "request",  request  )
        , ( "response", response )
        )

  theKey :: RiakKey <-
    maybe
      (maybe
        (nonsense "missing key")
        pure
        (coerce (response ^. #maybe'key)))
      pure
      key

  let
    loc :: RiakLocation 'Nothing
    loc =
      RiakLocation (RiakNamespace type' bucket) theKey

  -- Cache the vclock if asked for it with return_head or return_body.
  do
    let
      doCacheVclock :: ExceptT RiakError IO ()
      doCacheVclock =
        case response ^. #maybe'vclock of
          Nothing ->
            nonsense "missing vclock"

          Just theVclock ->
            cacheVclock handle loc (Just (coerce theVclock))

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
          pure theKey

        ParamObjectReturnHead ->
          traverse
            (parseContent @a proxy# loc STrue)
            (List1.fromList (response ^. #content))

        ParamObjectReturnBody ->
          traverse
            (parseContent @a proxy# loc SFalse)
            (List1.fromList (response ^. #content))

  lift theValue


-- TODO deleteRiakObject figure out when vclock is required (always?)
-- TODO don't use rw (deprecated)
deleteRiakObject
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RpbDelReq -- ^
  -> m (Either RiakError RpbDelResp)
deleteRiakObject (RiakHandle manager _) req = liftIO $
  withRiakConnection manager (\conn -> Internal.deleteRiakObject conn req)


-- | Fetch a counter.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
fetchRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just 'RiakCounterTy) -- ^
  -> FetchRiakObjectParams -- ^
  -> m (Either RiakError Int64)
fetchRiakCounter handle loc (FetchRiakObjectParams a b c d e f g) =
  fetchCrdt handle loc
    (FetchRiakCrdtParams a (IncludeContext Nothing) b c d e f g)


-- | Fetch a grow-only set.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain grow-only
--   sets.
fetchRiakGrowOnlySet
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just 'RiakGrowOnlySetTy) -- ^
  -> FetchRiakCrdtParams -- ^
  -> m (Either RiakError (Set ByteString))
fetchRiakGrowOnlySet =
  fetchCrdt


-- | Fetch a HyperLogLog.
--
-- Throws
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain HyperLogLogs.
fetchRiakHyperLogLog
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just 'RiakHyperLogLogTy) -- ^
  -> FetchRiakCrdtParams -- ^
  -> m (Either RiakError Word64)
fetchRiakHyperLogLog =
  fetchCrdt


-- | Fetch and decode a map.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain maps.
--
-- * 'RiakMapParseError' if decoding fails.
fetchRiakMap
  :: (IsRiakMap a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just ('RiakMapTy a)) -- ^
  -> FetchRiakCrdtParams -- ^
  -> m (Either RiakError a)
fetchRiakMap =
  fetchCrdt


-- | Fetch and decode a set.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain sets.
--
-- * 'SomeException' if decoding fails. The exception thrown is provided by the
--   implementation of 'decodeRiakRegister'.
fetchRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just ('RiakSetTy a)) -- ^
  -> FetchRiakCrdtParams -- ^
  -> m (Either RiakError (Set a))
fetchRiakSet =
  fetchCrdt


fetchCrdt
  :: forall m ty.
     (IsRiakCrdt ty, MonadIO m)
  => RiakHandle
  -> RiakLocation ('Just ty)
  -> FetchRiakCrdtParams
  -> m (Either RiakError (CrdtVal ty))
fetchCrdt
    handle@(RiakHandle manager _)
    loc@(RiakLocation (RiakNamespace type' bucket) key)
    (FetchRiakCrdtParams basic_quorum (IncludeContext include_context) n
      notfound_ok pr r sloppy_quorum timeout) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.fetchRiakCrdt conn request))

  case parseDtFetchResp loc response of
    Left err ->
      throwIO err

    Right value -> do
      -- Only counters don't have a causal context
      let
        shouldCache :: Bool
        shouldCache =
          response ^. #type' /= DtFetchResp'COUNTER &&
            include_context /= Just False

      when shouldCache $
        cacheVclock
          handle
          loc
          (coerce (response ^. #maybe'context))

      pure value

 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = unBasicQuorum basic_quorum
      , _DtFetchReq'bucket         = unRiakBucket bucket
      , _DtFetchReq'includeContext = include_context
      , _DtFetchReq'key            = unRiakKey key
      , _DtFetchReq'nVal           = coerce n
      , _DtFetchReq'notfoundOk     = unNotfoundOk notfound_ok
      , _DtFetchReq'pr             = coerce pr
      , _DtFetchReq'r              = coerce r
      , _DtFetchReq'sloppyQuorum   = coerce sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
      , _DtFetchReq'type'          = coerce type'
      }


-- | Update a counter and its updated value if @return_body@ is set, else 0.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
updateRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just 'RiakCounterTy) -- ^
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^
  -> m (Either RiakError Int64)
updateRiakCounter handle (RiakLocation namespace key) incr params =
  (fmap.fmap) snd (_updateRiakCounter handle namespace (Just key) incr params)


-- | Update a new counter and return its randomly-generated key.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
updateNewRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace ('Just 'RiakCounterTy) -- ^
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^
  -> m (Either RiakError RiakKey)
updateNewRiakCounter handle namespace incr params =
  (fmap.fmap) fst (_updateRiakCounter handle namespace Nothing incr params)

_updateRiakCounter
  :: MonadIO m
  => RiakHandle
  -> RiakNamespace ('Just 'RiakCounterTy)
  -> Maybe RiakKey
  -> Int64
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey, Int64))
_updateRiakCounter handle namespace key incr params =
  (fmap.fmap.fmap) (view #counterValue)
    (updateCrdt handle namespace key Nothing op params)
 where
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


-- | Update a set and return its updated value if @return_body@ is set, else
-- the empty set.
--
-- Throws a 'RiakCrdtError' if the given 'RiakLocation' does not contain
-- sets.
updateRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakLocation ('Just ('RiakSetTy a)) -- ^
  -> RiakSetOp a -- ^
  -> UpdateRiakCrdtParams -- ^
  -> m (Either RiakError (Set a))
updateRiakSet handle (RiakLocation namespace key) op params =
  (fmap.fmap) snd (_updateSet handle namespace (Just key) op params)

-- | Update a new set and return its randomly-generated key.
--
-- Throws a 'RiakCrdtError' if the given 'RiakLocation' does not contain
-- counters.
updateNewRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace ('Just ('RiakSetTy a)) -- ^
  -> RiakSetOp a -- ^
  -> UpdateRiakCrdtParams -- ^
  -> m (Either RiakError RiakKey)
updateNewRiakSet handle namespace op params =
  (fmap.fmap) fst (_updateSet handle namespace Nothing op params)

-- TODO _updateSet use same conn for get-put
_updateSet
  :: forall a m.
     (IsRiakSet a, MonadIO m)
  => RiakHandle
  -> RiakNamespace ('Just ('RiakSetTy a))
  -> Maybe RiakKey
  -> RiakSetOp a
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey, Set a))
_updateSet
    handle@(RiakHandle _ cache) namespace key (unSetOp -> (adds, removes))
    params = liftIO . runExceptT $ do

  -- Be a good citizen and perhaps include the cached causal context with this
  -- update request.
  --
  -- (1) If the set definitely doesn't already exist, no context is required.
  --
  -- (2) If the set might exist but we aren't attempting any removes, no
  --     context is required.
  --
  -- (3) If the set might exist and we are attempting at least one remove, a
  --     context is required (more like "recommended").
  --
  --     (a) If we have one cached, hooray.
  --
  --     (b) If we don't have one cached, perform a quick fetch first. Note that
  --         we won't get a context if the set doesn't exist - this would mean
  --         we are attempting to remove something from an empty set. That is
  --         weird, so I'm okay with not passing in a context in this situation
  --         (we don't have one to pass, anyway).

  context :: Maybe RiakVclock <-
    case key of
      -- (1)
      Nothing ->
        pure Nothing

      Just key' ->
        let
          loc :: RiakLocation ('Just ('RiakSetTy a))
          loc =
            RiakLocation namespace key'
        in do
          context :: Maybe RiakVclock <-
            lift (cacheLookup cache loc)

          if null removes
            then
              -- (2)
              pure context

            else
              case context of
                -- (3b)
                Nothing -> do
                  _ <- ExceptT (fetchRiakSet handle loc def)
                  lift (cacheLookup cache loc)

                -- (3a)
                Just context' ->
                  pure (Just context')

  (key', value) <-
    ExceptT (updateCrdt handle namespace key context op params)

  let
    loc :: RiakLocation ('Just ('RiakSetTy a))
    loc =
      RiakLocation namespace key'

  case parseDtUpdateResp loc value of
    Left err ->
      throwIO err

    Right value' ->
      pure (key', value')

 where
  op :: DtOp
  op =
    DtOp
      { _DtOp'_unknownFields = []
      , _DtOp'counterOp      = Nothing
      , _DtOp'gsetOp         = Nothing
      , _DtOp'hllOp          = Nothing
      , _DtOp'mapOp          = Nothing
      , _DtOp'setOp          = Just (Proto.SetOp adds removes [])
      }

updateCrdt
  :: MonadIO m
  => RiakHandle
  -> RiakNamespace ('Just ty)
  -> Maybe RiakKey
  -> Maybe RiakVclock
  -> DtOp
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey, DtUpdateResp))
updateCrdt
    (RiakHandle manager _) (RiakNamespace type' bucket) key context op
    (UpdateRiakCrdtParams dw n pw return_body sloppy_quorum timeout w) =
    liftIO . runExceptT $ do

  response :: DtUpdateResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.updateRiakCrdt conn request))

  theKey :: RiakKey <-
    maybe
      (maybe
        (panic "missing key"
          ( ( "request" , request  )
          , ( "response", response )
          ))
        pure
        (coerce (response ^. #maybe'key)))
      pure
      key

  pure (theKey, response)

 where
  request :: DtUpdateReq
  request =
    DtUpdateReq
      { _DtUpdateReq'_unknownFields = []
      , _DtUpdateReq'bucket         = unRiakBucket bucket
      , _DtUpdateReq'context        = coerce context
      , _DtUpdateReq'dw             = coerce dw
      , _DtUpdateReq'includeContext = Nothing
      , _DtUpdateReq'key            = coerce key
      , _DtUpdateReq'nVal           = coerce n
      , _DtUpdateReq'op             = op
      , _DtUpdateReq'pw             = coerce pw
      , _DtUpdateReq'returnBody     = unReturnBody return_body
      , _DtUpdateReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
      , _DtUpdateReq'timeout        = unTimeout timeout
      , _DtUpdateReq'type'          = unRiakBucketType type'
      , _DtUpdateReq'w              = coerce w
      }


-- TODO getRiakBucketTypeProps return BucketProps
getRiakBucketTypeProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakBucketType ty -- ^
  -> m (Either RiakError RpbBucketProps)
getRiakBucketTypeProps (RiakHandle manager _) type' = liftIO . runExceptT $ do
  response :: RpbGetBucketResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.getRiakBucketTypeProps conn request))
  pure (response ^. #props)

 where
  request :: RpbGetBucketTypeReq
  request =
    RpbGetBucketTypeReq
      { _RpbGetBucketTypeReq'_unknownFields = []
      , _RpbGetBucketTypeReq'type'          = unRiakBucketType type'
      }


-- TODO: Don't allow setting n
setRiakBucketTypeProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakBucketType ty -- ^
  -> RpbBucketProps -- ^
  -> m (Either RiakError ())
setRiakBucketTypeProps (RiakHandle manager _) type' props =
  liftIO (fmap (() <$)
    (withRiakConnection manager
      (\conn -> (Internal.setRiakBucketTypeProps conn request))))
 where
  request :: RpbSetBucketTypeReq
  request =
    RpbSetBucketTypeReq
      { _RpbSetBucketTypeReq'_unknownFields = []
      , _RpbSetBucketTypeReq'props          = props
      , _RpbSetBucketTypeReq'type'          = unRiakBucketType type'
      }


-- TODO getRiakBucketProps return BucketProps
getRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakNamespace ty -- ^
  -> m (Either RiakError RpbBucketProps)
getRiakBucketProps (RiakHandle manager _) (RiakNamespace type' bucket) = liftIO . runExceptT $ do
  response :: RpbGetBucketResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.getRiakBucketProps conn request))
  pure (response ^. #props)

 where
  request :: RpbGetBucketReq
  request =
    RpbGetBucketReq
      { _RpbGetBucketReq'_unknownFields = []
      , _RpbGetBucketReq'bucket         = unRiakBucket bucket
      , _RpbGetBucketReq'type'          = Just (unRiakBucketType type')
      }


-- TODO: setRiakBucketProps Don't allow setting n
setRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RpbSetBucketReq -- ^
  -> m (Either RiakError ())
setRiakBucketProps (RiakHandle manager _) req =
  liftIO (fmap (() <$)
    (withRiakConnection manager
      (\conn -> Internal.setRiakBucketProps conn req)))


resetRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RpbResetBucketReq -- ^
  -> m (Either RiakError ())
resetRiakBucketProps (RiakHandle manager _) req = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> Internal.resetRiakBucketProps conn req))


-- TODO listRiakBuckets param timeout
listRiakBuckets
  :: RiakHandle -- ^ Riak handle.
  -> RiakBucketType ty -- ^
  -> (ListT (ExceptT RiakError IO) RiakBucket -> IO r)
  -> IO r
listRiakBuckets (RiakHandle manager _) type' k =
  withRiakConnection manager $ \conn -> k $ do
    response :: RpbListBucketsResp <-
      Internal.listRiakBuckets conn request

    ListT.select (coerce (response ^. #buckets) :: [RiakBucket])

 where
  request :: RpbListBucketsReq
  request =
    RpbListBucketsReq
      { _RpbListBucketsReq'_unknownFields = []
      , _RpbListBucketsReq'stream = Just True
      , _RpbListBucketsReq'timeout = Nothing
      , _RpbListBucketsReq'type' = Just (unRiakBucketType type')
      }

-- TODO listRiakKeys param timeout
listRiakKeys
  :: RiakHandle -- ^ Riak handle.
  -> RiakNamespace ty -- ^
  -> (ListT (ExceptT RiakError IO) RiakKey -> IO r)
  -> IO r
listRiakKeys (RiakHandle manager _) (RiakNamespace type' bucket) k =
  withRiakConnection manager $ \conn -> k $ do
    response :: RpbListKeysResp <-
      Internal.listRiakKeys conn request

    ListT.select (coerce (response ^. #keys) :: [RiakKey])

 where
  request :: RpbListKeysReq
  request =
    RpbListKeysReq
      { _RpbListKeysReq'_unknownFields = []
      , _RpbListKeysReq'bucket         = unRiakBucket bucket
      , _RpbListKeysReq'timeout        = Nothing
      , _RpbListKeysReq'type'          = Just (unRiakBucketType type')
      }

riakMapReduce
  :: RiakHandle -- ^ Riak handle.
  -> RpbMapRedReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbMapRedResp -> IO r)
  -> IO r
riakMapReduce (RiakHandle manager _) request k =
  withRiakConnection manager $ \conn -> k $
    Internal.riakMapReduce conn request


getRiakSchema
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakSchemaName -- ^
  -> m (Either RiakError RpbYokozunaSchemaGetResp)
getRiakSchema (RiakHandle manager _) schema = liftIO $
  withRiakConnection manager
    (\conn -> Internal.getRiakSchema conn request)
 where
  request :: RpbYokozunaSchemaGetReq
  request =
    RpbYokozunaSchemaGetReq
      { _RpbYokozunaSchemaGetReq'_unknownFields = []
      , _RpbYokozunaSchemaGetReq'name           = unRiakSchemaName schema
      }


putRiakSchema
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakSchemaName -- ^
  -> ByteString -- ^
  -> m (Either RiakError ())
putRiakSchema (RiakHandle manager _) name bytes = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> Internal.putRiakSchema conn request))
 where
  request :: RpbYokozunaSchemaPutReq
  request =
    RpbYokozunaSchemaPutReq
      { _RpbYokozunaSchemaPutReq'_unknownFields = []
      , _RpbYokozunaSchemaPutReq'schema         =
          RpbYokozunaSchema
            { _RpbYokozunaSchema'_unknownFields = []
            , _RpbYokozunaSchema'content        = Just bytes
            , _RpbYokozunaSchema'name           = unRiakSchemaName name
            }
      }


getRiakIndex
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakIndexName -- ^
  -> m (Either RiakError (Maybe RpbYokozunaIndex))
getRiakIndex (RiakHandle manager _) name = liftIO . runExceptT $ do
  ExceptT
    (withRiakConnection manager
      (\conn -> Internal.getRiakIndex conn request)) >>= \case

    RpbYokozunaIndexGetResp [] _ ->
      pure Nothing
    RpbYokozunaIndexGetResp (index:_) _ ->
      pure (Just index)

 where
  request :: RpbYokozunaIndexGetReq
  request =
    RpbYokozunaIndexGetReq
      { _RpbYokozunaIndexGetReq'_unknownFields = []
      , _RpbYokozunaIndexGetReq'name           = Just (unRiakIndexName name)
      }


getRiakIndexes
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> m (Either RiakError [RpbYokozunaIndex])
getRiakIndexes (RiakHandle manager _) = liftIO . runExceptT $ do
  RpbYokozunaIndexGetResp indexes _ <-
    ExceptT
      (withRiakConnection manager
        (\conn -> Internal.getRiakIndex conn request))
  pure indexes

 where
  request :: RpbYokozunaIndexGetReq
  request =
    RpbYokozunaIndexGetReq
      { _RpbYokozunaIndexGetReq'_unknownFields = []
      , _RpbYokozunaIndexGetReq'name           = Nothing
      }


putRiakIndex
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RiakIndexName -- ^
  -> RiakSchemaName -- ^
  -> m (Either RiakError ())
putRiakIndex (RiakHandle manager _) index schema = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> Internal.putRiakIndex conn request))
 where
  request :: RpbYokozunaIndexPutReq
  request =
    RpbYokozunaIndexPutReq
      { _RpbYokozunaIndexPutReq'_unknownFields  = []
      , _RpbYokozunaIndexPutReq'index           =
          RpbYokozunaIndex
             { _RpbYokozunaIndex'_unknownFields = []
             , _RpbYokozunaIndex'nVal           = Nothing -- TODO putRiakIndex n_val
             , _RpbYokozunaIndex'name           = unRiakIndexName index
             , _RpbYokozunaIndex'schema         = Just (unRiakSchemaName schema)
             }
      , _RpbYokozunaIndexPutReq'timeout         = Nothing -- TODO putRiakIndex timeout
      }


deleteRiakIndex
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> RpbYokozunaIndexDeleteReq -- ^
  -> m (Either RiakError RpbDelResp)
deleteRiakIndex (RiakHandle manager _) req = liftIO $
  withRiakConnection manager
    (\conn -> Internal.deleteRiakIndex conn req)


pingRiak
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> m (Either RiakError ())
pingRiak (RiakHandle manager _) = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> Internal.pingRiak conn))


getRiakServerInfo
  :: MonadIO m
  => RiakHandle -- ^ Riak handle.
  -> m (Either RiakError RpbGetServerInfoResp)
getRiakServerInfo (RiakHandle manager _) = liftIO $
  withRiakConnection manager
    (\conn -> Internal.getRiakServerInfo conn)


--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

-- | Given a fetched vclock, update the cache (if present) or delete it from the
-- cache (if missing).
cacheVclock
  :: MonadIO m
  => RiakHandle
  -> RiakLocation ty
  -> Maybe RiakVclock
  -> m ()
cacheVclock (RiakHandle _ cache) loc =
  liftIO . maybe (cacheDelete cache loc) (cacheInsert cache loc)


parseContent
  :: forall a head.
     IsRiakContent a
  => Proxy# a
  -> RiakLocation 'Nothing
  -> SBool head
  -> RpbContent
  -> IO (RiakContent (If head (Proxy a) a))
parseContent _ loc head
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
          (decodeRiakContent
            (coerce content_type)
            (coerce charset)
            (coerce content_encoding)
            value)

  let
    theLastMod :: Maybe NominalDiffTime
    theLastMod = do
      secs  <- last_mod
      usecs <- last_mod_usecs <|> pure 0
      let usecs_d = realToFrac usecs / 1000000 :: Double
      pure (fromIntegral secs + realToFrac usecs_d)

  pure $ RiakContent
    loc
    theValue
    (coerce content_type)
    (coerce charset)
    (coerce content_encoding)
    (coerce vtag)
    (posixSecondsToUTCTime <$> theLastMod)
    (RiakMetadata (map unRpbPair usermeta))
    (map rpbPairToIndex indexes)
    (fromMaybe False deleted)
    (TTL ttl)


indexToRpbPair :: RiakSecondaryIndex -> RpbPair
indexToRpbPair = \case
  RiakSecondaryIndexInt k v ->
    RpbPair k (Just (Latin1.pack (show v))) [] -- TODO more efficient show

  RiakSecondaryIndexBin k v ->
    RpbPair k (Just v) []


rpbPairToIndex :: RpbPair -> RiakSecondaryIndex
rpbPairToIndex = \case
  RpbPair (ByteString.stripSuffix "_bin" -> Just k) (Just v) _ ->
    RiakSecondaryIndexBin k v

  RpbPair
      (ByteString.stripSuffix "_int" -> Just k)
      (Just (readMaybe . Latin1.unpack -> Just v)) _ ->
    RiakSecondaryIndexInt k v -- TODO better read

  -- TODO what to do if index value is empty...?
  _ ->
    undefined


rpbPair :: (ByteString, Maybe ByteString) -> RpbPair
rpbPair (k, v) =
  RpbPair k v []


unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
unRpbPair (RpbPair k v _) =
  (k, v)

unSetOp :: IsRiakSet a => RiakSetOp a -> ([ByteString], [ByteString])
unSetOp (RiakSetOp (adds, removes)) =
  ( map encodeRiakRegister (toList adds)
  , map encodeRiakRegister (toList removes)
  )


-- $documentation
--
-- = Bucket types
--
-- 'RiakBucketType's carry a type-level tag that indicates what kind of data is
-- stored within:
--
-- +----------------------------+-----------------+
-- | @Nothing@                  | Opaque objects. |
-- +----------------------------+-----------------+
-- | @Just 'RiakCounterTy'@     | Counters.       |
-- +----------------------------+-----------------+
-- | @Just 'RiakGrowOnlySetTy'@ | Grow-only sets. |
-- +----------------------------+-----------------+
-- | @Just 'RiakHyperLogLogTy'@ | HyperLogLogs.   |
-- +----------------------------+-----------------+
-- | @Just 'RiakMapTy'@         | Maps.           |
-- +----------------------------+-----------------+
-- | @Just 'RiakSetTy'@         | Sets.           |
-- +----------------------------+-----------------+
--
-- You should know ahead of time what kind of data corresponds to each bucket
-- type, and may define these bucket types as top-level definitions.
--
-- For example,
--
-- @
-- countersBucketType :: 'RiakBucketType' ''CounterTy'
-- countersBucketType = 'RiakBucketType' "counters"
-- @
--
-- = Content
--
-- TODO write this
--
-- = Optional parameters
--
-- Each request takes a bundle of optional parameters as a data type named
-- @*Params@. It is always constructed using 'def' and overloaded labels syntax.
--
-- For example, 'FetchRiakObjectParams' has an instance
--
-- @
-- IsLabel "sloppy_quorum" (Bool -> FetchRiakObjectParams -> FetchRiakObjectParams)
-- @
--
-- To use this instance, write
--
-- @
-- def & #sloppy_quorum False
-- @
--
-- = @vclock@ cache
--
-- Riak objects carry a causal context (either a vclock or a dotted version
-- vector) that help Riak resolve some conflicts automatically.
--
-- Proper handling of the causal context is fairly simple: to perform a write,
-- first perform a read to fetch the causal context, then include it in the
-- write request. This will minimize, but not eliminate, the creation of
-- siblings.
--
-- This library caches every vclock fetched and includes them in write requests
-- automatically. Siblings are /not/ automatically resolved: if you
-- ever read siblings, it is your responsibility to resolve them in your
-- application and perform a followup write to eliminate them.
--
-- Riak data types, which cannot have siblings, also have causal contexts that
-- are cached and included in write requests automatically.
--
-- = Glossary
--
-- [__allow mult__]
-- Whether siblings can be created. The legacy default bucket type defaults to
-- false, but other bucket types default to false, and false is the recommended
-- setting for most use cases.
--
-- [__basic quorum__]
-- Whether to use the "basic quorum" policy for not-founds. Only relevant when
-- __notfound_ok__ is set to false.
--
--     * /Default/: false.
--
-- [__dw__]
-- The number of vnodes that must write a write request to storage before a
-- response is returned to the client. The request will still be replicated to
-- __n__ vnodes.
--
--     * /Default/: @quorum@.
--
--     * /Range/: 1 to __n__.
--
-- [__last write wins__]
-- Resolve conflicts with timestamps. Only relevant if __allow mult__ is false,
-- which is not recommended.
--
-- [__n__]
-- The number of /primary vnodes/ responsible for each key, i.e. the number of
-- /replicas/ stores in the cluster.
--
--     * /Default/: 3.
--
--     * /Range/: 1 to the number of nodes in the cluster.
--
-- [__notfound ok__]
-- Controls how Riak behaves during read requests when keys are not present. If
-- @true@, Riak will treat any @notfound@ as a positive assertion that the key
-- does not exist. If @false@, Riak will treat any @notfound@ as a failure
-- condition. The coordinating node will wait for enough vnodes to reply with
-- @notfound@ to know that it cannot satisfy the requested __r__.
--
--     * /Default/: true.
--
-- [__pr__]
-- The number of primary vnodes that must respond to a read request before a
-- response is returned to the client. The request will still be replicated to
-- __n__ vnodes.
--
--     * /Default/: 0.
--
--     * /Range/: 1 to __n__.
--
-- [__pw__]
-- The number of primary vnodes that must /respond/ to a write request before a
-- response is returned to the client. The request will still be replicated to
-- __n__ vnodes.
--
--     * /Default/: 0.
--
--     * /Range/: 1 to __n__.
--
-- [__r__]
-- The number of vnodes that must respond to a read request before a response is
-- returned to the client. The request will still be replicated to __n__ vnodes.
--
--     * /Default/: @quorum@.
--
--     * /Range/: 1 to __n__.
--
-- [__sloppy quorum__]
-- Whether failover vnodes are consulted if one or more primary vnodes fails.
--
-- * /Default/: true.
--
-- [__w__]
-- The number of vnodes that must /respond/ to a write request before a response
-- is returned to the client. The request will still be replicated to __n__
-- vnodes.
--
--     * /Default/: @quorum@.
--
--     * /Range/: 1 to __nN__.
