{-# LANGUAGE UndecidableInstances #-}

module Riak
  ( -- * Handle
    createRiakHandle
    -- * Object operations
    -- ** Get object
  , getRiakObject
  , getRiakObjectHead
  , getRiakObjectIfModified
  , getRiakObjectIfModifiedHead
    -- ** Put object
  , putRiakObject
  , putRiakObjectHead
  , putRiakObjectBody
  , putNewRiakObject
  , putNewRiakObjectHead
  , putNewRiakObjectBody
    -- ** Delete object
  , deleteRiakObject
    -- * Counter operations
    -- ** Get counter
  , getRiakCounter
    -- ** Update counter
  , updateRiakCounter
  , updateNewRiakCounter
    -- * Grow-only set operations
    -- ** Get grow-only set
  , getRiakGrowOnlySet
    -- ** Update grow-only set
    -- * HyperLogLog operations
    -- ** Get HyperLogLog
  , getRiakHyperLogLog
    -- ** Update HyperLogLog
    -- * Set operations
    -- ** Get set
  , getRiakSet
    -- ** Update set
  , updateRiakSet
  , updateNewRiakSet
  , riakSetAddOp
  , riakSetRemoveOp
    -- * Map operations
  , getRiakMap
    -- ** Get map
  , riakCounterField
  , riakFlagField
  , riakMapField
  , riakRegisterField
  , riakSetField
    -- ** Update map
  -- , allowExtraKeys
    -- * Bucket operations
  , getRiakBucketTypeProps
  , setRiakBucketTypeProps
  , getRiakBucketProps
  , setRiakBucketProps
  , resetRiakBucketProps
    -- * Full key traversals
  , streamRiakBuckets
  , streamRiakKeys
    -- * MapReduce
    -- $mapreduce
  , riakMapReduceBucket
  , riakMapReduceKeys
  , riakMapReduceFunction
  , riakMapReduceExactQuery
  , riakMapReduceRangeQuery
  , riakMapReducePhaseMapIdentity
  , riakMapReducePhaseMapObjectValue
  , riakMapReducePhaseReduceCount
  , riakMapReducePhaseReduceSetUnion
  , riakMapReducePhaseReduceSort
  , riakMapReducePhaseReduceSum
    -- * Secondary indexes (2i)
    -- $secondary-indexes
  , riakExactQuery
  , riakRangeQuery
  , riakRangeQueryTerms
    -- * Search 2.0
    -- ** Search
  , riakSearch
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
  , GetRiakCrdtParams
  , GetRiakObjectParams
  , IsRiakMap(..)
  , IsRiakObject(..)
  , IsRiakRegister(..)
  , IsRiakSet
  , JsonRiakObject(..)
  , Modified(..)
  , PutRiakObjectParams
  , RiakBucket(..)
  , pattern DefaultRiakBucket
  , RiakBucketType(..)
  , pattern DefaultRiakBucketType
  , RiakCrdtError(..)
  , RiakCrdtTy(..)
  , RiakError(..)
  , RiakExactQuery(..)
  , RiakHandle
  , RiakIndex(..)
  , RiakIndexName(..)
  , RiakKey(..)
  , RiakMapEntries(..)
  , RiakMapFieldParser
  , RiakMapParseError(..)
  , RiakMapReduceInputs(..)
  , RiakMapReducePhase(..)
  , RiakMetadata(..)
  , RiakObject(..)
  , RiakQuorum(..)
  , pattern RiakQuorumAll
  , pattern RiakQuorumQuorum
  , RiakRangeQuery(..)
  , RiakSearchParams
  , RiakSetOp
  , RiakVtag(..)
  , RpbMapRedResp(..)
  , SolrIndexName(..)
  , pattern DontIndex
  , SolrSchemaName(..)
  , pattern DefaultSolrSchemaName
  , TTL(..)
  , UpdateRiakCrdtParams
    -- * Re-exports
  , def
  , HostName
  , PortNumber
    -- * Documentation
    -- $documentation
  ) where

import Control.Foldl         (FoldM)
import Data.Default.Class    (def)
import Data.Profunctor       (lmap)
import Data.Time             (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Labels
import Network.Socket        (HostName, PortNumber)

import qualified Control.Foldl         as Foldl
import qualified Data.ByteString       as ByteString
import qualified Data.HashSet          as HashSet
import qualified Data.List.NonEmpty    as List1

import           Proto.Riak              hiding (SetOp)
import qualified Proto.Riak              as Proto
import           Riak.Internal
import           Riak.Internal.Cache     (newSTMRiakCache)
import           Riak.Internal.Crdts     (CrdtVal, IsRiakCrdt(..))
import           Riak.Internal.MapReduce (riakMapReduceRequest)
import           Riak.Internal.Panic
import           Riak.Internal.Prelude
import           Riak.Internal.Types     (ObjectReturn(..),
                                          ParamObjectReturn(..), SBool(..))
import           Riak.Internal.Utils     (bs2int, int2bs)


-- TODO _ variants that don't decode replies

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | Create a handle to Riak with the following default settings, which may be
-- overridden by using various functionality exposed in the "Riak.Internal"
-- module.
--
-- * Object and data type causal contexts are cached using an in-memory
--   map from @stm-containers@.
--
-- * A maximum of two OS sockets per capability are opened.
--
-- * Inactive sockets are closed after approximately 30 seconds.
--
-- TODO createRiakHandle close connections after
createRiakHandle
  :: MonadIO m
  => HostName -- ^ Host
  -> PortNumber -- ^ Port
  -> m RiakHandle
createRiakHandle host port = liftIO $ do
  cache :: RiakCache <-
    newSTMRiakCache

  sockets :: Int <-
    (* 2) <$> getNumCapabilities

  manager :: RiakManager <-
    createRiakManager host port sockets 30

  pure (RiakHandle manager cache)


--------------------------------------------------------------------------------
-- Get object
--------------------------------------------------------------------------------

-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
--
-- === __Examples__
--
-- Get a 'Text' object at bucket type @"t"@, bucket @"b"@, key @"k"@. Note that
-- the type application @\@Text@ is only necessary when type inference fails.
--
-- @
-- do
--   result <-
--     'getRiakObject' @'Text' h ('RiakKey' "t" "b" "k") 'def'
--
--   case result of
--     'Left' err ->
--       {- Some Riak error -}
--
--     'Right' 'Nothing' ->
--       {- The object does not exist -}
--
--     'Right' object ->
--       {- The object does exist -}
-- @
getRiakObject
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError [RiakObject a])
getRiakObject
    h@(RiakHandle manager _)
    loc@(RiakKey (RiakBucket (RiakBucketType type') bucket) key)
    (GetRiakObjectParams basic_quorum n notfound_ok pr r sloppy_quorum timeout)
    = liftIO . runExceptT $ do

  response :: RpbGetResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakObjectPB conn request))

  contents :: [RiakObject a] <- lift $
    traverse
      (parseContent' loc)
      (filter notTombstone (response ^. #content))

  case contents of
    c0 : _ | contentIsDataType c0 ->
      pure ()

    _ -> do
      lift (cacheVclock h loc (coerce (response ^. #maybe'vclock)))

  pure contents

 where
  request :: RpbGetReq
  request =
    RpbGetReq
      { _RpbGetReq'_unknownFields = []
      , _RpbGetReq'basicQuorum    = unBasicQuorum basic_quorum
      , _RpbGetReq'bucket         = bucket
      , _RpbGetReq'deletedvclock  = Just True
      , _RpbGetReq'head           = Nothing
      , _RpbGetReq'ifModified     = Nothing
      , _RpbGetReq'key            = key
      , _RpbGetReq'nVal           = coerce n
      , _RpbGetReq'notfoundOk     = unNotfoundOk notfound_ok
      , _RpbGetReq'pr             = coerce pr
      , _RpbGetReq'r              = coerce r
      , _RpbGetReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
      , _RpbGetReq'timeout        = unTimeout timeout
      , _RpbGetReq'type'          = Just type'
      }


-- | Get an object's metadata.
getRiakObjectHead
    :: forall m.
       MonadIO m
    => RiakHandle -- ^ Riak handle
    -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
    -> GetRiakObjectParams -- ^ Optional parameters
    -> m (Either RiakError [RiakObject ()])
getRiakObjectHead
    h@(RiakHandle manager _)
    loc@(RiakKey (RiakBucket (RiakBucketType type') bucket) key)
    (GetRiakObjectParams basic_quorum n notfound_ok pr r sloppy_quorum timeout)
    = liftIO . runExceptT $ do

  response :: RpbGetResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakObjectPB conn request))

  contents :: [RiakObject ()] <- lift $
    traverse
      (parseContentHead loc)
      (filter notTombstone (response ^. #content))

  case contents of
    c0 : _ | contentIsDataType c0 ->
      pure ()

    _ -> do
      lift (cacheVclock h loc (coerce (response ^. #maybe'vclock)))

  pure contents

 where
  request :: RpbGetReq
  request =
    RpbGetReq
      { _RpbGetReq'_unknownFields = []
      , _RpbGetReq'basicQuorum    = unBasicQuorum basic_quorum
      , _RpbGetReq'bucket         = bucket
      , _RpbGetReq'deletedvclock  = Just True
      , _RpbGetReq'head           = Just True
      , _RpbGetReq'ifModified     = Nothing
      , _RpbGetReq'key            = key
      , _RpbGetReq'nVal           = coerce n
      , _RpbGetReq'notfoundOk     = unNotfoundOk notfound_ok
      , _RpbGetReq'pr             = coerce pr
      , _RpbGetReq'r              = coerce r
      , _RpbGetReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
      , _RpbGetReq'timeout        = unTimeout timeout
      , _RpbGetReq'type'          = Just type'
      }

-- | Get an object if it has been modified since the last get.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
getRiakObjectIfModified
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (Modified [RiakObject a]))
getRiakObjectIfModified
    h@(RiakHandle manager cache)
    loc@(RiakKey (RiakBucket (RiakBucketType type') bucket) key)
    (GetRiakObjectParams basic_quorum n notfound_ok pr r sloppy_quorum timeout)
    = liftIO . runExceptT $ do

  vclock :: Maybe RiakVclock <-
    lift (riakCacheLookup cache loc)

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = unBasicQuorum basic_quorum
        , _RpbGetReq'bucket         = bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           = Nothing
        , _RpbGetReq'ifModified     = coerce vclock
        , _RpbGetReq'key            = key
        , _RpbGetReq'nVal           = coerce n
        , _RpbGetReq'notfoundOk     = unNotfoundOk notfound_ok
        , _RpbGetReq'pr             = coerce pr
        , _RpbGetReq'r              = coerce r
        , _RpbGetReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
        , _RpbGetReq'timeout        = unTimeout timeout
        , _RpbGetReq'type'          = Just type'
        }

  response :: RpbGetResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakObjectPB conn request))

  if response ^. #unchanged
    then
      pure Unmodified

    else do
      contents :: [RiakObject a] <- lift $
        traverse
          (parseContent' loc)
          (filter notTombstone (response ^. #content))

      case contents of
        c0 : _ | contentIsDataType c0 ->
          pure ()

        _ -> do
          lift (cacheVclock h loc (coerce (response ^. #maybe'vclock)))

      pure (Modified contents)

-- | Get an object's metadata if it has been modified since the last get.
getRiakObjectIfModifiedHead
  :: forall m.
     MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (Modified [RiakObject ()]))
getRiakObjectIfModifiedHead
    h@(RiakHandle manager cache)
    loc@(RiakKey (RiakBucket (RiakBucketType type') bucket) key)
    (GetRiakObjectParams basic_quorum n notfound_ok pr r sloppy_quorum timeout)
    = liftIO . runExceptT $ do

  vclock :: Maybe RiakVclock <-
    lift (riakCacheLookup cache loc)

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = unBasicQuorum basic_quorum
        , _RpbGetReq'bucket         = bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           = Nothing
        , _RpbGetReq'ifModified     = coerce vclock
        , _RpbGetReq'key            = key
        , _RpbGetReq'nVal           = coerce n
        , _RpbGetReq'notfoundOk     = unNotfoundOk notfound_ok
        , _RpbGetReq'pr             = coerce pr
        , _RpbGetReq'r              = coerce r
        , _RpbGetReq'sloppyQuorum   = unSloppyQuorum sloppy_quorum
        , _RpbGetReq'timeout        = unTimeout timeout
        , _RpbGetReq'type'          = Just type'
        }

  response :: RpbGetResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakObjectPB conn request))

  if response ^. #unchanged
    then
      pure Unmodified

    else do
      contents :: [RiakObject ()] <- lift $
        traverse
          (parseContentHead loc)
          (filter notTombstone (response ^. #content))

      case contents of
        c0 : _ | contentIsDataType c0 ->
          pure ()

        _ -> do
          lift (cacheVclock h loc (coerce (response ^. #maybe'vclock)))

      pure (Modified contents)

--------------------------------------------------------------------------------
-- Put object
--------------------------------------------------------------------------------

type family ObjectReturnTy (a :: Type) (return :: ObjectReturn) where
  ObjectReturnTy _ 'ObjectReturnNone = RiakKey 'Nothing
  ObjectReturnTy _ 'ObjectReturnHead = NonEmpty (RiakObject ())
  ObjectReturnTy a 'ObjectReturnBody = NonEmpty (RiakObject a)

-- | Put an object.
putRiakObject
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError ())
putRiakObject
    handle (RiakKey bucket key) content (PutRiakObjectParams a b c d e f g h) =
  fmap (() <$)
    (_putRiakObject handle bucket (Just key) content a b c d e
      ParamObjectReturnNone f g h)

-- | Put an object and return its metadata.
putRiakObjectHead
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (NonEmpty (RiakObject ())))
putRiakObjectHead
    handle (RiakKey bucket key) content (PutRiakObjectParams a b c d e f g h) =
  (_putRiakObject handle bucket (Just key) content a b c d e
    ParamObjectReturnHead f g h)

-- | Put an object and return it.
putRiakObjectBody
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (NonEmpty (RiakObject a)))
putRiakObjectBody
    handle (RiakKey bucket key) content (PutRiakObjectParams a b c d e f g h) =
  _putRiakObject handle bucket (Just key) content a b c d e
    ParamObjectReturnBody f g h

-- | Put a new object and return its randomly-generated key.
putNewRiakObject
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (RiakKey 'Nothing))
putNewRiakObject
    handle bucket content (PutRiakObjectParams a b c d e f g h) =
  _putRiakObject handle bucket Nothing content a b c d e
    ParamObjectReturnNone f g h

-- | Put an new object and return its metadata.
putNewRiakObjectHead
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (RiakObject ()))
putNewRiakObjectHead
    handle bucket content (PutRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head
    (_putRiakObject handle bucket Nothing content a b c d e
      ParamObjectReturnHead f g h)

-- | Put an new object and return it.
putNewRiakObjectBody
  :: forall a m.
     (IsRiakObject a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (RiakObject a))
putNewRiakObjectBody
    handle bucket content (PutRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head $
    _putRiakObject handle bucket Nothing content a b c d e
      ParamObjectReturnBody f g h

_putRiakObject
  :: forall a m return.
     (IsRiakObject a, MonadIO m)
  => RiakHandle
  -> RiakBucket 'Nothing
  -> Maybe ByteString
  -> a
  -> DW
  -> [RiakIndex]
  -> RiakMetadata
  -> N
  -> PW
  -> ParamObjectReturn return
  -> SloppyQuorum
  -> Timeout
  -> W
  -> m (Either RiakError (ObjectReturnTy a return))
_putRiakObject
    h@(RiakHandle manager cache) namespace@(RiakBucket type' bucket) key value
    dw indexes metadata n pw return sloppy_quorum timeout w = liftIO . runExceptT $ do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe RiakVclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . riakCacheLookup cache . RiakKey namespace)
      key

  let
    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = Nothing
        , _RpbPutReq'bucket         = bucket
        , _RpbPutReq'content        =
            RpbContent
              { _RpbContent'_unknownFields  = []
              , _RpbContent'charset         = coerce (riakObjectCharset value)
              , _RpbContent'contentEncoding = coerce (riakObjectContentEncoding value)
              , _RpbContent'contentType     = coerce (riakObjectContentType value)
              , _RpbContent'deleted         = Nothing
              , _RpbContent'indexes         = map indexToRpbPair (coerce indexes)
              , _RpbContent'lastMod         = Nothing
              , _RpbContent'lastModUsecs    = Nothing
              , _RpbContent'links           = []
              , _RpbContent'ttl             = Nothing
              , _RpbContent'usermeta        = map rpbPair (coerce metadata)
              , _RpbContent'value           = encodeRiakObject value
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
        (\conn -> putRiakObjectPB conn request))

  let
    nonsense :: Text -> ExceptT RiakError IO void
    nonsense s =
      panic s
        ( ( "request",  request  )
        , ( "response", response )
        )

  theKey :: ByteString <-
    maybe
      (maybe
        (nonsense "missing key")
        pure
        (response ^. #maybe'key))
      pure
      key

  let
    loc :: RiakKey 'Nothing
    loc =
      RiakKey namespace theKey

  -- Cache the vclock if asked for it with return_head or return_body.
  do
    let
      doCacheVclock :: ExceptT RiakError IO ()
      doCacheVclock =
        case response ^. #maybe'vclock of
          Nothing ->
            nonsense "missing vclock"

          Just theVclock ->
            cacheVclock h loc (Just (coerce theVclock))

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
          pure loc

        ParamObjectReturnHead ->
          traverse
            (parseContent @a proxy# loc STrue)
            (List1.fromList (response ^. #content))

        ParamObjectReturnBody ->
          traverse
            (parseContent @a proxy# loc SFalse)
            (List1.fromList (response ^. #content))

  lift theValue


--------------------------------------------------------------------------------
-- Delete object
--------------------------------------------------------------------------------

-- | Delete an object or data type.
deleteRiakObject
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey ty -- ^ Bucket type, bucket, and key
  -> m (Either RiakError ())
deleteRiakObject
    (RiakHandle manager cache)
    loc@(RiakKey (RiakBucket (RiakBucketType type') bucket) key) = liftIO $ do

  vclock :: Maybe RiakVclock <-
    riakCacheLookup cache loc

  let
    request :: RpbDelReq
    request =
      RpbDelReq
        { _RpbDelReq'_unknownFields = []
        , _RpbDelReq'bucket         = bucket
        , _RpbDelReq'dw             = Nothing
        , _RpbDelReq'key            = key
        , _RpbDelReq'nVal           = Nothing
        , _RpbDelReq'pr             = Nothing
        , _RpbDelReq'pw             = Nothing
        , _RpbDelReq'r              = Nothing
        , _RpbDelReq'rw             = Nothing
        , _RpbDelReq'sloppyQuorum   = Nothing
        , _RpbDelReq'timeout        = Nothing
        , _RpbDelReq'type'          = Just type'
        , _RpbDelReq'vclock         = coerce vclock
        , _RpbDelReq'w              = Nothing
        }

  fmap (() <$)
    (withRiakConnection manager (\conn -> deleteRiakObjectPB conn request))


--------------------------------------------------------------------------------
-- Get counter
--------------------------------------------------------------------------------

-- TODO: make updates throw RiakCrdtError as well

-- | Get a counter.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakKey' does not contain counters.
getRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just 'RiakCounterTy) -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError Int64)
getRiakCounter h key (GetRiakObjectParams a b c d e f g) =
  getCrdt h key (GetRiakCrdtParams a (IncludeContext Nothing) b c d e f g)


--------------------------------------------------------------------------------
-- Update counter
--------------------------------------------------------------------------------

-- | Update a counter and its updated value if @return_body@ is set, else 0.
updateRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just 'RiakCounterTy) -- ^ Bucket type, bucket, and key
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError Int64)
updateRiakCounter h (RiakKey bucket key) incr params =
  (fmap.fmap) snd (_updateRiakCounter h bucket (Just key) incr params)


-- | Update a new counter and return its randomly-generated key.
updateNewRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakBucket ('Just 'RiakCounterTy) -- ^
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (RiakKey ('Just 'RiakCounterTy)))
updateNewRiakCounter h bucket incr params =
  (fmap.fmap) fst (_updateRiakCounter h bucket Nothing incr params)

_updateRiakCounter
  :: MonadIO m
  => RiakHandle
  -> RiakBucket ('Just 'RiakCounterTy)
  -> Maybe ByteString
  -> Int64
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey ('Just 'RiakCounterTy), Int64))
_updateRiakCounter h bucket key incr params =
  (fmap.fmap.fmap) (view #counterValue)
    (updateCrdt h bucket key Nothing op params)
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


--------------------------------------------------------------------------------
-- Get grow-only set
--------------------------------------------------------------------------------

-- | Get a grow-only set.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakKey' does not contain grow-only
--   sets.
getRiakGrowOnlySet
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just 'RiakGrowOnlySetTy) -- ^ Bucket type, bucket, and key
  -> GetRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (Set ByteString))
getRiakGrowOnlySet =
  getCrdt


--------------------------------------------------------------------------------
-- Update grow-only set
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Get HyperLogLog
--------------------------------------------------------------------------------

-- | Get a HyperLogLog.
--
-- Throws
--
-- * 'RiakCrdtError' if the given 'RiakKey' does not contain HyperLogLogs.
getRiakHyperLogLog
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just 'RiakHyperLogLogTy) -- ^ Bucket type, bucket, and key
  -> GetRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError Word64)
getRiakHyperLogLog =
  getCrdt


--------------------------------------------------------------------------------
-- Update HyperLogLog
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Get map
--------------------------------------------------------------------------------

-- | Get and decode a map.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakKey' does not contain maps.
--
-- * 'RiakMapParseError' if decoding fails.
getRiakMap
  :: (IsRiakMap a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just ('RiakMapTy a)) -- ^ Bucket type, bucket, and key
  -> GetRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError a)
getRiakMap =
  getCrdt


-------------------------------------------------------------------------------
-- Update map
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Get set
--------------------------------------------------------------------------------

-- | Get and decode a set.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakKey' does not contain sets.
--
-- * 'SomeException' if decoding fails. The exception thrown is provided by the
--   implementation of 'decodeRiakRegister'.
getRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just ('RiakSetTy a)) -- ^ Bucket type, bucket, and key
  -> GetRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (Set a))
getRiakSet =
  getCrdt


-------------------------------------------------------------------------------
-- Update set
--------------------------------------------------------------------------------

-- | Update a set and return its updated value if @return_body@ is set, else
-- the empty set.
updateRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakKey ('Just ('RiakSetTy a)) -- ^ Bucket type, bucket, and key
  -> RiakSetOp a -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (Set a))
updateRiakSet h (RiakKey bucket key) op params =
  (fmap.fmap) snd (_updateSet h bucket (Just key) op params)

-- | Update a new set and return its randomly-generated key.
updateNewRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakBucket ('Just ('RiakSetTy a)) -- ^ Bucket type and bucket
  -> RiakSetOp a -- ^ Set operation
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (RiakKey ('Just ('RiakSetTy a))))
updateNewRiakSet h bucket op params =
  (fmap.fmap) fst (_updateSet h bucket Nothing op params)

-- TODO _updateSet use same conn for get-put
_updateSet
  :: forall a m.
     (IsRiakSet a, MonadIO m)
  => RiakHandle
  -> RiakBucket ('Just ('RiakSetTy a))
  -> Maybe ByteString
  -> RiakSetOp a
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey ('Just ('RiakSetTy a)), Set a))
_updateSet
    h@(RiakHandle _ cache) namespace key (unSetOp -> (adds, removes))
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
  --     (b) If we don't have one cached, perform a quick get first. Note that
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
          loc :: RiakKey ('Just ('RiakSetTy a))
          loc =
            RiakKey namespace key'
        in do
          context :: Maybe RiakVclock <-
            lift (riakCacheLookup cache loc)

          if null removes
            then
              -- (2)
              pure context

            else
              case context of
                -- (3b)
                Nothing -> do
                  _ <- ExceptT (getRiakSet h loc def)
                  lift (riakCacheLookup cache loc)

                -- (3a)
                Just context' ->
                  pure (Just context')

  (loc, response) <-
    ExceptT (updateCrdt h namespace key context op params)

  case parseDtUpdateResp loc response of
    Left err ->
      throwIO err

    Right value -> do
      -- If the context was fetched, cache it
      for_ (response ^. #maybe'context) $ \vclock ->
        cacheVclock h loc (Just (RiakVclock vclock))

      pure (loc, value)

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


--------------------------------------------------------------------------------
-- Get data type
--------------------------------------------------------------------------------

getCrdt
  :: forall m ty.
     (IsRiakCrdt ty, MonadIO m)
  => RiakHandle
  -> RiakKey ('Just ty)
  -> GetRiakCrdtParams
  -> m (Either RiakError (CrdtVal ty))
getCrdt
    h@(RiakHandle manager _) loc@(RiakKey (RiakBucket type' bucket) key)
    (GetRiakCrdtParams basic_quorum (IncludeContext include_context) n
      notfound_ok pr r sloppy_quorum timeout) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakCrdtPB conn request))

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
          h
          loc
          (coerce (response ^. #maybe'context))

      pure value

 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = unBasicQuorum basic_quorum
      , _DtFetchReq'bucket         = bucket
      , _DtFetchReq'includeContext = include_context
      , _DtFetchReq'key            = key
      , _DtFetchReq'nVal           = coerce n
      , _DtFetchReq'notfoundOk     = unNotfoundOk notfound_ok
      , _DtFetchReq'pr             = coerce pr
      , _DtFetchReq'r              = coerce r
      , _DtFetchReq'sloppyQuorum   = coerce sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
      , _DtFetchReq'type'          = unRiakBucketType type'
      }


-------------------------------------------------------------------------------
-- Update data type
--------------------------------------------------------------------------------

updateCrdt
  :: MonadIO m
  => RiakHandle
  -> RiakBucket ('Just ty)
  -> Maybe ByteString
  -> Maybe RiakVclock
  -> DtOp
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey ('Just ty), DtUpdateResp))
updateCrdt
    (RiakHandle manager _) namespace@(RiakBucket type' bucket) key context op
    (UpdateRiakCrdtParams dw n pw return_body sloppy_quorum timeout w) =
    liftIO . runExceptT $ do

  response :: DtUpdateResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> updateRiakCrdtPB conn request))

  theKey :: ByteString <-
    maybe
      (maybe
        (panic "missing key"
          ( ( "request" , request  )
          , ( "response", response )
          ))
        pure
        (response ^. #maybe'key))
      pure
      key

  pure (RiakKey namespace theKey, response)

 where
  request :: DtUpdateReq
  request =
    DtUpdateReq
      { _DtUpdateReq'_unknownFields = []
      , _DtUpdateReq'bucket         = bucket
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


--------------------------------------------------------------------------------
-- Get bucket type properties
--------------------------------------------------------------------------------

-- TODO getRiakBucketTypeProps return BucketProps
getRiakBucketTypeProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakBucketType ty -- ^ Bucket type
  -> m (Either RiakError RpbBucketProps)
getRiakBucketTypeProps (RiakHandle manager _) type' = liftIO . runExceptT $ do
  response :: RpbGetBucketResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakBucketTypePropsPB conn request))
  pure (response ^. #props)

 where
  request :: RpbGetBucketTypeReq
  request =
    RpbGetBucketTypeReq
      { _RpbGetBucketTypeReq'_unknownFields = []
      , _RpbGetBucketTypeReq'type'          = unRiakBucketType type'
      }


--------------------------------------------------------------------------------
-- Set bucket type properties
--------------------------------------------------------------------------------

-- TODO: Don't allow setting n
setRiakBucketTypeProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakBucketType ty -- ^ Bucket type
  -> RpbBucketProps -- ^
  -> m (Either RiakError ())
setRiakBucketTypeProps (RiakHandle manager _) type' props =
  liftIO (fmap (() <$)
    (withRiakConnection manager
      (\conn -> (setRiakBucketTypePropsPB conn request))))
 where
  request :: RpbSetBucketTypeReq
  request =
    RpbSetBucketTypeReq
      { _RpbSetBucketTypeReq'_unknownFields = []
      , _RpbSetBucketTypeReq'props          = props
      , _RpbSetBucketTypeReq'type'          = unRiakBucketType type'
      }


--------------------------------------------------------------------------------
-- Get bucket props
--------------------------------------------------------------------------------

-- TODO getRiakBucketProps return BucketProps
getRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakBucket ty -- ^ Bucket type and bucket
  -> m (Either RiakError RpbBucketProps)
getRiakBucketProps (RiakHandle manager _) (RiakBucket type' bucket) = liftIO . runExceptT $ do
  response :: RpbGetBucketResp <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakBucketPropsPB conn request))
  pure (response ^. #props)

 where
  request :: RpbGetBucketReq
  request =
    RpbGetBucketReq
      { _RpbGetBucketReq'_unknownFields = []
      , _RpbGetBucketReq'bucket         = bucket
      , _RpbGetBucketReq'type'          = Just (unRiakBucketType type')
      }


--------------------------------------------------------------------------------
-- Set bucket props
--------------------------------------------------------------------------------

-- TODO: setRiakBucketProps Don't allow setting n
setRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RpbSetBucketReq -- ^
  -> m (Either RiakError ())
setRiakBucketProps (RiakHandle manager _) req =
  liftIO (fmap (() <$)
    (withRiakConnection manager
      (\conn -> setRiakBucketPropsPB conn req)))


--------------------------------------------------------------------------------
-- Reset bucket props
--------------------------------------------------------------------------------

resetRiakBucketProps
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RpbResetBucketReq -- ^
  -> m (Either RiakError ())
resetRiakBucketProps (RiakHandle manager _) req = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> resetRiakBucketPropsPB conn req))


--------------------------------------------------------------------------------
-- List buckets
--------------------------------------------------------------------------------

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- TODO streamRiakBuckets param timeout
streamRiakBuckets
  :: forall r ty.
     RiakHandle -- ^ Riak handle
  -> RiakBucketType ty -- ^ Bucket type
  -> FoldM IO (RiakBucket ty) r
  -> IO (Either RiakError r)
streamRiakBuckets (RiakHandle manager _) type' fold =
  withRiakConnection manager $ \conn -> do
    streamRiakBuckets2PB conn request
      (fold
        & Foldl.handlesM Foldl.folded
        & lmap (map (RiakBucket type') . (^. #buckets)))
 where
  request :: RpbListBucketsReq
  request =
    RpbListBucketsReq
      { _RpbListBucketsReq'_unknownFields = []
      , _RpbListBucketsReq'stream = Just True
      , _RpbListBucketsReq'timeout = Nothing
      , _RpbListBucketsReq'type' = Just (unRiakBucketType type')
      }


--------------------------------------------------------------------------------
-- List keys
--------------------------------------------------------------------------------

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- TODO streamRiakKeys param timeout
streamRiakKeys
  :: RiakHandle -- ^ Riak handle
  -> RiakBucket ty -- ^ Bucket type and bucket
  -> FoldM IO (RiakKey ty) r -- ^
  -> IO (Either RiakError r)
streamRiakKeys (RiakHandle manager _) namespace@(RiakBucket type' bucket) fold =
  withRiakConnection manager $ \conn -> do
    streamRiakKeysPB conn request
      (fold
        & Foldl.handlesM Foldl.folded
        & lmap (map (RiakKey namespace) . (^. #keys)))

 where
  request :: RpbListKeysReq
  request =
    RpbListKeysReq
      { _RpbListKeysReq'_unknownFields = []
      , _RpbListKeysReq'bucket         = bucket
      , _RpbListKeysReq'timeout        = Nothing
      , _RpbListKeysReq'type'          = Just (unRiakBucketType type')
      }


--------------------------------------------------------------------------------
-- MapReduce
--------------------------------------------------------------------------------

-- $mapreduce
--
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/usage/mapreduce/>
--
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/app-guide/advanced-mapreduce/>
--
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/mapreduce/>

riakMapReduceBucket
  :: RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
riakMapReduceBucket h bucket =
  _riakMapReduce h (RiakMapReduceInputsBucket bucket)

riakMapReduceKeys
  :: RiakHandle -- ^ Riak handle
  -> [RiakKey 'Nothing] -- ^ Bucket types, buckets, and keys
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
riakMapReduceKeys h keys =
  _riakMapReduce h (RiakMapReduceInputsKeys keys)

riakMapReduceFunction
  :: RiakHandle -- ^ Riak handle
  -> Text -- ^ Erlang module.
  -> Text -- ^ Erlang function.
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
riakMapReduceFunction h m f =
  _riakMapReduce h (RiakMapReduceInputsFunction m f)

riakMapReduceExactQuery
  :: RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket.
  -> RiakExactQuery -- ^ Exact query.
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
riakMapReduceExactQuery h bucket query =
  _riakMapReduce h (RiakMapReduceInputsExactQuery bucket query)

riakMapReduceRangeQuery
  :: RiakHandle -- ^ Riak handle
  -> RiakBucket 'Nothing -- ^ Bucket type and bucket.
  -> RiakRangeQuery a -- ^ Range query.
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
riakMapReduceRangeQuery h bucket query =
  _riakMapReduce h (RiakMapReduceInputsRangeQuery bucket query)

_riakMapReduce
  :: RiakHandle -- ^ Riak handle
  -> RiakMapReduceInputs -- ^ MapReduce inputs
  -> [RiakMapReducePhase] -- ^ MapReduce phases
  -> FoldM IO RpbMapRedResp r -- ^
  -> IO (Either RiakError r)
_riakMapReduce (RiakHandle manager _) inputs query k =
  withRiakConnection manager $ \conn ->
    riakMapReducePB conn (riakMapReduceRequest inputs query) k

--------------------------------------------------------------------------------
-- Secondary indexes
--------------------------------------------------------------------------------

-- $secondary-indexes
--
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/usage/secondary-indexes/>
--
-- * <http://docs.basho.com/riak/kv/2.2.3/using/reference/secondary-indexes/>
--
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/secondary-indexes/>

-- TODO 2i query paging

-- | Perform an exact query on a secondary index.
riakExactQuery
  :: RiakHandle -- ^
  -> RiakBucket 'Nothing -- ^
  -> RiakExactQuery -- ^
  -> FoldM IO (RiakKey 'Nothing) r -- ^
  -> IO (Either RiakError r)
riakExactQuery
    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
  withRiakConnection manager $ \conn ->
    riakIndexPB conn request
      (fold
        & Foldl.handlesM Foldl.folded
        & lmap (map (RiakKey namespace) . (^. #keys)))
 where
  request :: RpbIndexReq
  request =
    RpbIndexReq
      { _RpbIndexReq'_unknownFields = []
      , _RpbIndexReq'bucket         = bucket
      , _RpbIndexReq'continuation   = Nothing
      , _RpbIndexReq'coverContext   = Nothing
      , _RpbIndexReq'index          = unRiakIndexName index
      , _RpbIndexReq'key            = Just key
      , _RpbIndexReq'maxResults     = Nothing
      , _RpbIndexReq'paginationSort = Nothing
      , _RpbIndexReq'qtype          = RpbIndexReq'eq
      , _RpbIndexReq'rangeMax       = Nothing
      , _RpbIndexReq'rangeMin       = Nothing
      , _RpbIndexReq'returnBody     = Nothing
      , _RpbIndexReq'returnTerms    = Nothing
      , _RpbIndexReq'stream         = Just True
      , _RpbIndexReq'termRegex      = Nothing
      , _RpbIndexReq'timeout        = Nothing
      , _RpbIndexReq'type'          = Just (unRiakBucketType type')
      }

  index :: RiakIndexName
  index =
    case query of
      RiakExactQueryBin (RiakIndexName idx) _ ->
        RiakIndexName (idx <> "_bin")

      RiakExactQueryInt (RiakIndexName idx) _ ->
        RiakIndexName (idx <> "_int")

  key :: ByteString
  key =
    case query of
      RiakExactQueryBin _ n -> n
      RiakExactQueryInt _ n -> int2bs n


-- | Perform a range query on a secondary index.
riakRangeQuery
  :: RiakHandle -- ^
  -> RiakBucket 'Nothing -- ^
  -> RiakRangeQuery a -- ^
  -> FoldM IO (RiakKey 'Nothing) r -- ^
  -> IO (Either RiakError r)
riakRangeQuery
    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
  withRiakConnection manager $ \conn ->
    riakIndexPB conn request
      (fold
        & Foldl.handlesM Foldl.folded
        & lmap (map (RiakKey namespace) . (^. #keys)))
 where
  request :: RpbIndexReq
  request =
    RpbIndexReq
      { _RpbIndexReq'_unknownFields = []
      , _RpbIndexReq'bucket         = bucket
      , _RpbIndexReq'continuation   = Nothing
      , _RpbIndexReq'coverContext   = Nothing
      , _RpbIndexReq'index          = unRiakIndexName index
      , _RpbIndexReq'key            = Nothing
      , _RpbIndexReq'maxResults     = Nothing
      , _RpbIndexReq'paginationSort = Nothing
      , _RpbIndexReq'qtype          = RpbIndexReq'range
      , _RpbIndexReq'rangeMax       = Just rmax
      , _RpbIndexReq'rangeMin       = Just rmin
      , _RpbIndexReq'returnBody     = Nothing
      , _RpbIndexReq'returnTerms    = Nothing
      , _RpbIndexReq'stream         = Just True
      , _RpbIndexReq'termRegex      = Nothing
      , _RpbIndexReq'timeout        = Nothing
      , _RpbIndexReq'type'          = Just (unRiakBucketType type')
      }

  index :: RiakIndexName
  index =
    case query of
      RiakRangeQueryBin (RiakIndexName idx) _ _ ->
        RiakIndexName (idx <> "_bin")
      RiakRangeQueryInt (RiakIndexName idx) _ _ ->
        RiakIndexName (idx <> "_int")

  rmin :: ByteString
  rmin =
    case query of
      RiakRangeQueryBin _ n _ -> n
      RiakRangeQueryInt _ n _ -> int2bs n

  rmax :: ByteString
  rmax =
    case query of
      RiakRangeQueryBin _ _ n -> n
      RiakRangeQueryInt _ _ n -> int2bs n

-- | Perform a range query on a secondary index, and return the indexed terms
-- along with the keys.
riakRangeQueryTerms
  :: forall a r.
     RiakHandle -- ^
  -> RiakBucket 'Nothing -- ^
  -> RiakRangeQuery a -- ^
  -> FoldM IO (RiakKey 'Nothing, a) r -- ^
  -> IO (Either RiakError r)
riakRangeQueryTerms
    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
  withRiakConnection manager $ \conn ->
    riakIndexPB conn request
      (fold
        & Foldl.handlesM Foldl.folded
        & lmap (map parse . (^. #results)))
 where
  request :: RpbIndexReq
  request =
    RpbIndexReq
      { _RpbIndexReq'_unknownFields = []
      , _RpbIndexReq'bucket         = bucket
      , _RpbIndexReq'continuation   = Nothing
      , _RpbIndexReq'coverContext   = Nothing
      , _RpbIndexReq'index          = unRiakIndexName index
      , _RpbIndexReq'key            = Nothing
      , _RpbIndexReq'maxResults     = Nothing
      , _RpbIndexReq'paginationSort = Nothing
      , _RpbIndexReq'qtype          = RpbIndexReq'range
      , _RpbIndexReq'rangeMax       = Just rmax
      , _RpbIndexReq'rangeMin       = Just rmin
      , _RpbIndexReq'returnBody     = Nothing
      , _RpbIndexReq'returnTerms    = Just True
      , _RpbIndexReq'stream         = Just True
      , _RpbIndexReq'termRegex      = Nothing
      , _RpbIndexReq'timeout        = Nothing
      , _RpbIndexReq'type'          = Just (unRiakBucketType type')
      }

  index :: RiakIndexName
  index =
    case query of
      RiakRangeQueryBin (RiakIndexName idx) _ _ ->
        RiakIndexName (idx <> "_bin")
      RiakRangeQueryInt (RiakIndexName idx) _ _ ->
        RiakIndexName (idx <> "_int")

  rmin :: ByteString
  rmin =
    case query of
      RiakRangeQueryBin _ n _ -> n
      RiakRangeQueryInt _ n _ -> int2bs n

  rmax :: ByteString
  rmax =
    case query of
      RiakRangeQueryBin _ _ n -> n
      RiakRangeQueryInt _ _ n -> int2bs n

  parse :: RpbPair -> (RiakKey 'Nothing, a)
  parse = \case
    RpbPair val (Just key) _ ->
      (RiakKey namespace key, parse1 val)
    _ ->
      undefined

  parse1 :: ByteString -> a
  parse1 =
    case query of
      RiakRangeQueryBin _ _ _ -> id
      RiakRangeQueryInt _ _ _ -> bs2int



--------------------------------------------------------------------------------
-- Search 2.0
--------------------------------------------------------------------------------

-- | Execute a search.
riakSearch
  :: RiakHandle -- ^ Riak handle
  -> ByteString -- ^ Query
  -> ByteString -- ^ Index
  -> RiakSearchParams -- ^ Optional parameters
  -> IO (Either RiakError RpbSearchQueryResp)
riakSearch (RiakHandle manager _)
    query index
    (RiakSearchParams df filter' fl op presort rows sort start) =
  withRiakConnection manager $ \conn ->
    riakSearchPB conn request
 where
  request :: RpbSearchQueryReq
  request =
    RpbSearchQueryReq
      { _RpbSearchQueryReq'_unknownFields = []
      , _RpbSearchQueryReq'df             = unDF df
      , _RpbSearchQueryReq'filter         = unFilter filter'
      , _RpbSearchQueryReq'fl             = unFL fl
      , _RpbSearchQueryReq'index          = index
      , _RpbSearchQueryReq'op             = unOp op
      , _RpbSearchQueryReq'presort        = unPresort presort
      , _RpbSearchQueryReq'q              = query
      , _RpbSearchQueryReq'rows           = unRows rows
      , _RpbSearchQueryReq'sort           = unSort sort
      , _RpbSearchQueryReq'start          = unStart start
      }


getRiakSchema
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> SolrSchemaName -- ^ Schema name
  -> m (Either RiakError (Maybe RpbYokozunaSchemaGetResp))
getRiakSchema (RiakHandle manager _) schema = liftIO $
  withRiakConnection manager
    (\conn ->
      translateNotfound <$> getRiakSchemaPB conn request)
 where
  request :: RpbYokozunaSchemaGetReq
  request =
    RpbYokozunaSchemaGetReq
      { _RpbYokozunaSchemaGetReq'_unknownFields = []
      , _RpbYokozunaSchemaGetReq'name           = unSolrSchemaName schema
      }


putRiakSchema
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> SolrSchemaName -- ^ Schema name
  -> ByteString -- ^ Schema contents
  -> m (Either RiakError ())
putRiakSchema (RiakHandle manager _) name bytes = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> putRiakSchemaPB conn request))
 where
  request :: RpbYokozunaSchemaPutReq
  request =
    RpbYokozunaSchemaPutReq
      { _RpbYokozunaSchemaPutReq'_unknownFields = []
      , _RpbYokozunaSchemaPutReq'schema         =
          RpbYokozunaSchema
            { _RpbYokozunaSchema'_unknownFields = []
            , _RpbYokozunaSchema'content        = Just bytes
            , _RpbYokozunaSchema'name           = unSolrSchemaName name
            }
      }


getRiakIndex
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakIndexName -- ^
  -> m (Either RiakError (Maybe RpbYokozunaIndex))
getRiakIndex (RiakHandle manager _) name = liftIO $ do
  withRiakConnection manager (runExceptT . runMaybeT . action)
 where
  request :: RpbYokozunaIndexGetReq
  request =
    RpbYokozunaIndexGetReq
      { _RpbYokozunaIndexGetReq'_unknownFields = []
      , _RpbYokozunaIndexGetReq'name           = Just (unRiakIndexName name)
      }

  action
    :: RiakConnection
    -> MaybeT (ExceptT RiakError IO) RpbYokozunaIndex
  action conn =
    MaybeT (ExceptT m) >>= \case
      RpbYokozunaIndexGetResp [index] _ ->
        pure index
      response ->
        panic "0 or 2+ indexes"
          ( ( "request",  request  )
          , ( "response", response )
          )

   where
    m :: IO (Either RiakError (Maybe RpbYokozunaIndexGetResp))
    m =
      translateNotfound <$> getRiakIndexPB conn request

getRiakIndexes
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> m (Either RiakError [RpbYokozunaIndex])
getRiakIndexes (RiakHandle manager _) = liftIO . runExceptT $ do
  RpbYokozunaIndexGetResp indexes _ <-
    ExceptT
      (withRiakConnection manager
        (\conn -> getRiakIndexPB conn request))
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
  => RiakHandle -- ^ Riak handle
  -> RiakIndexName -- ^
  -> SolrSchemaName -- ^
  -> m (Either RiakError ())
putRiakIndex (RiakHandle manager _) index schema = liftIO $
  fmap (() <$)
    (withRiakConnection manager
      (\conn -> putRiakIndexPB conn request))
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
             , _RpbYokozunaIndex'schema         = Just (unSolrSchemaName schema)
             }
      , _RpbYokozunaIndexPutReq'timeout         = Nothing -- TODO putRiakIndex timeout
      }


deleteRiakIndex
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RpbYokozunaIndexDeleteReq -- ^
  -> m (Either RiakError RpbDelResp)
deleteRiakIndex (RiakHandle manager _) req = liftIO $
  withRiakConnection manager
    (\conn -> deleteRiakIndexPB conn req)


--------------------------------------------------------------------------------
-- Server info
--------------------------------------------------------------------------------

pingRiak
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> m (Either RiakError ())
pingRiak (RiakHandle manager _) = liftIO $
  fmap (() <$) (withRiakConnection manager pingRiakPB)


getRiakServerInfo
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> m (Either RiakError RpbGetServerInfoResp)
getRiakServerInfo (RiakHandle manager _) = liftIO $
  withRiakConnection manager
    (\conn -> getRiakServerInfoPB conn)


--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

-- | Given a fetched vclock, update the cache (if present) or delete it from the
-- cache (if missing).
cacheVclock
  :: MonadIO m
  => RiakHandle
  -> RiakKey ty
  -> Maybe RiakVclock
  -> m ()
cacheVclock (RiakHandle _ cache) loc =
  liftIO . maybe (riakCacheDelete cache loc) (riakCacheInsert cache loc)


contentIsDataType :: RiakObject a -> Bool
contentIsDataType =
  maybe False (`elem` crdtContentTypes) . (^. #contentType)

crdtContentTypes :: HashSet ContentType
crdtContentTypes =
  HashSet.fromList
    [ ContentType "application/riak_counter"
    , ContentType "application/riak_gset"
    , ContentType "application/riak_hll"
    , ContentType "application/riak_map"
    , ContentType "application/riak_set"
    ]


notTombstone :: RpbContent -> Bool
notTombstone content =
  not (content ^. #deleted)



parseContent
  :: forall a head.
     IsRiakObject a
  => Proxy# a
  -> RiakKey 'Nothing
  -> SBool head
  -> RpbContent
  -> IO (RiakObject (If head () a))
parseContent _ loc head
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  let
    theLastMod :: Maybe NominalDiffTime
    theLastMod = do
      secs  <- last_mod
      usecs <- last_mod_usecs <|> pure 0
      let usecs_d = realToFrac usecs / 1000000 :: Double
      pure (fromIntegral secs + realToFrac usecs_d)

  let
    rawObject :: RiakObject ByteString
    rawObject =
      RiakObject
        loc
        value
        (coerce content_type)
        (coerce charset)
        (coerce content_encoding)
        (coerce vtag)
        (posixSecondsToUTCTime <$> theLastMod)
        (RiakMetadata (map unRpbPair usermeta))
        (map rpbPairToIndex indexes)
        (fromMaybe False deleted)
        (TTL ttl)

  theValue :: If head () a <-
    case head of
      STrue ->
        pure ()

      SFalse ->
        either throwIO pure (decodeRiakObject rawObject)

  pure (theValue <$ rawObject)

-- TODO replace parseContent with parseContent', parseContentHead

parseContent'
  :: forall a.
     IsRiakObject a
  => RiakKey 'Nothing
  -> RpbContent
  -> IO (RiakObject a)
parseContent' =
  _parseContent decodeRiakObject

parseContentHead
  :: RiakKey 'Nothing
  -> RpbContent
  -> IO (RiakObject ())
parseContentHead =
  _parseContent (\_ -> Right ())


_parseContent
  :: forall a.
     (RiakObject ByteString -> Either SomeException a)
  -> RiakKey 'Nothing
  -> RpbContent
  -> IO (RiakObject a)
_parseContent parse loc
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  let
    theLastMod :: Maybe NominalDiffTime
    theLastMod = do
      secs  <- last_mod
      usecs <- last_mod_usecs <|> pure 0
      let usecs_d = realToFrac usecs / 1000000 :: Double
      pure (fromIntegral secs + realToFrac usecs_d)

  let
    rawObject :: RiakObject ByteString
    rawObject =
      RiakObject
        loc
        value
        (coerce content_type)
        (coerce charset)
        (coerce content_encoding)
        (coerce vtag)
        (posixSecondsToUTCTime <$> theLastMod)
        (RiakMetadata (map unRpbPair usermeta))
        (map rpbPairToIndex indexes)
        (fromMaybe False deleted)
        (TTL ttl)

  theValue :: a <-
    either throwIO pure (parse rawObject)

  pure (theValue <$ rawObject)


indexToRpbPair :: RiakIndex -> RpbPair
indexToRpbPair = \case
  RiakIndexInt k v ->
    RpbPair
      (unRiakIndexName k <> "_int")
      (Just (int2bs v))
      []

  RiakIndexBin k v ->
    RpbPair
      (unRiakIndexName k <> "_bin")
      (Just v)
      []


rpbPairToIndex :: RpbPair -> RiakIndex
rpbPairToIndex = \case
  RpbPair (ByteString.stripSuffix "_bin" -> Just k) (Just v) _ ->
    RiakIndexBin (RiakIndexName k) v

  RpbPair (ByteString.stripSuffix "_int" -> Just k) (Just v) _ ->
    RiakIndexInt (RiakIndexName k) (bs2int v)

  RpbPair k v _ ->
    impurePanic "rpbPairToIndex"
      ( ("key",   k)
      , ("value", v)
      )

translateNotfound :: Either RiakError a -> Either RiakError (Maybe a)
translateNotfound = \case
  Left (RiakError "notfound") -> Right Nothing
  Left x                      -> Left x
  Right x                     -> Right (Just x)

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
-- For example, 'GetRiakObjectParams' has an instance
--
-- @
-- IsLabel "sloppy_quorum" (Bool -> GetRiakObjectParams -> GetRiakObjectParams)
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
-- false, but other bucket types default to true, and true is the recommended
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
-- /replicas/ stored in the cluster.
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
--     * /Range/: 1 to __n__.
