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
    -- * Data type operations
    -- ** Counter
  , getRiakCounter
  , updateRiakCounter
  , updateNewRiakCounter
    -- ** Grow-only set
  , getRiakGrowOnlySet
    -- ** HyperLogLog
  , getRiakHyperLogLog
    -- ** Set
  , getRiakSet
  , updateRiakSet
  , updateNewRiakSet
  , riakSetAddOp
  , riakSetRemoveOp
    -- ** Map
  , getRiakMap
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
    -- * Full key traversals
  , streamRiakBuckets
  , listRiakBuckets
  , streamRiakKeys
  , listRiakKeys
    -- * MapReduce
  , riakMapReduce
    -- * Secondary indexes (2i)
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
  , IsRiakContent(..)
  , IsRiakMap(..)
  , IsRiakRegister(..)
  , IsRiakSet
  , JsonRiakContent(..)
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
  , PutRiakObjectParams
  , TTL(..)
  , UpdateRiakCrdtParams
    -- * Re-exports
  , def
  , HostName
  , PortNumber
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

import           Proto.Riak            hiding (SetOp)
import qualified Proto.Riak            as Proto
import           Riak.Internal
import           Riak.Internal.Cache   (newSTMRiakCache)
import           Riak.Internal.Crdts   (CrdtVal, IsRiakCrdt(..))
import           Riak.Internal.Panic
import           Riak.Internal.Prelude
import           Riak.Internal.Types   (Head(..), IfModified(..),
                                        ObjectReturn(..), ParamObjectReturn(..),
                                        SBool(..))


-- TODO _ variants that don't decode replies
-- TODO rename "content" to "object"

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- TODO createRiakHandle close connections after
createRiakHandle
  :: MonadIO m
  => HostName -- ^ Host
  -> PortNumber -- ^ Port
  -> m RiakHandle
createRiakHandle host port = do
  cache :: RiakCache <-
    liftIO newSTMRiakCache

  manager :: RiakManager <-
    liftIO (createRiakManager host port)

  pure (RiakHandle manager cache)


--------------------------------------------------------------------------------
-- Get object
--------------------------------------------------------------------------------

-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
getRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError [RiakContent a])
getRiakObject handle loc (GetRiakObjectParams a b c d e f g) =
  _getRiakObject handle loc a NoHead NoIfModified b c d e f g

-- | Get an object's metadata.
getRiakObjectHead
    :: forall m.
       MonadIO m
    => RiakHandle -- ^ Riak handle
    -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
    -> GetRiakObjectParams -- ^ Optional parameters
    -> m (Either RiakError [RiakContent ()])
getRiakObjectHead handle loc (GetRiakObjectParams a b c d e f g) =
  -- TODO remove this fmap.fmap.fmap once _getRiakObject is refactored
  (fmap.fmap.fmap) (() <$)
    (_getRiakObject @ByteString handle loc a Head NoIfModified b c d e f g)

-- | Get an object if it has been modified since the last get.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
getRiakObjectIfModified
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (Modified [RiakContent a]))
getRiakObjectIfModified handle loc (GetRiakObjectParams a b c d e f g) =
  _getRiakObject handle loc a NoHead IfModified b c d e f g

-- | Get an object's metadata if it has been modified since the last get.
getRiakObjectIfModifiedHead
  :: forall m.
     MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (Modified [RiakContent ()]))
getRiakObjectIfModifiedHead handle loc (GetRiakObjectParams a b c d e f g) =
  (fmap.fmap.fmap.fmap) (() <$)
    (_getRiakObject @ByteString handle loc a Head IfModified b c d e f g)

type GetRiakObjectResp (head :: Bool) (if_modified :: Bool) (a :: Type)
  = IfModifiedWrapper if_modified [(RiakContent (If head () a))]

type family IfModifiedWrapper (if_modified :: Bool) (a :: Type) where
  IfModifiedWrapper 'True  a = Modified a
  IfModifiedWrapper 'False a = a

-- TODO delete _getRiakObject and inline its logic in the 4 variants?
_getRiakObject
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
  -> m (Either RiakError (GetRiakObjectResp head if_modified a))
_getRiakObject
    h@(RiakHandle manager cache)
    loc@(RiakLocation (RiakNamespace type' bucket) key)
    basic_quorum head if_modified n notfound_ok pr r sloppy_quorum timeout =
    liftIO . runExceptT $ do

  vclock :: Maybe RiakVclock <-
    case if_modified of
      IfModified   -> lift (riakCacheLookup cache loc)
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
              NoHead -> Nothing
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
        (\conn -> getRiakObjectPB conn request))

  -- Only cache the vclock if we didn't received an "unmodified" response (which
  -- doesn't contain a vclock)
  case (if_modified, response ^. #maybe'unchanged) of
    (IfModified, Just True) ->
      pure ()
    _ -> do
      cacheVclock h loc (coerce (response ^. #maybe'vclock))

  lift (mkResponse response)

 where
  mkResponse
    :: RpbGetResp
    -> IO (GetRiakObjectResp head if_modified a)
  mkResponse (RpbGetResp (filter notTombstone -> content) _ unchanged _) =
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
    contents :: IO [RiakContent (If head () a)]
    contents =
      traverse (parseContent @a proxy# loc headAsBool) content
     where
      headAsBool :: SBool head
      headAsBool =
        case head of
          Head   -> STrue
          NoHead -> SFalse

      -- headAsBool' :: Bool
      -- headAsBool' =
      --   case head of
      --     Head -> True
      --     NoHead -> False


--------------------------------------------------------------------------------
-- Put object
--------------------------------------------------------------------------------

type family ObjectReturnTy (a :: Type) (return :: ObjectReturn) where
  ObjectReturnTy _ 'ObjectReturnNone = RiakKey
  ObjectReturnTy _ 'ObjectReturnHead = NonEmpty (RiakContent ())
  ObjectReturnTy a 'ObjectReturnBody = NonEmpty (RiakContent a)

-- | Put an object.
putRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError ())
putRiakObject
    handle (RiakLocation namespace key) content (PutRiakObjectParams a b c d e f g h) =
  fmap (() <$)
    (_putRiakObject handle namespace (Just key) content a b c d e
      ParamObjectReturnNone f g h)

-- | Put an object and return its metadata.
putRiakObjectHead
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (NonEmpty (RiakContent ())))
putRiakObjectHead
    handle (RiakLocation namespace key) content (PutRiakObjectParams a b c d e f g h) =
  (_putRiakObject handle namespace (Just key) content a b c d e
    ParamObjectReturnHead f g h)

-- | Put an object and return it.
putRiakObjectBody
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation 'Nothing -- ^ Bucket type, bucket, and key
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (NonEmpty (RiakContent a)))
putRiakObjectBody
    handle (RiakLocation namespace key) content (PutRiakObjectParams a b c d e f g h) =
  _putRiakObject handle namespace (Just key) content a b c d e
    ParamObjectReturnBody f g h

-- | Put a new object and return its randomly-generated key.
putNewRiakObject
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakNamespace 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError RiakKey)
putNewRiakObject
    handle namespace content (PutRiakObjectParams a b c d e f g h) =
  _putRiakObject handle namespace Nothing content a b c d e
    ParamObjectReturnNone f g h

-- | Put an new object and return its metadata.
putNewRiakObjectHead
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakNamespace 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (RiakContent ()))
putNewRiakObjectHead
    handle namespace content (PutRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head
    (_putRiakObject handle namespace Nothing content a b c d e
      ParamObjectReturnHead f g h)

-- | Put an new object and return it.
putNewRiakObjectBody
  :: forall a m.
     (IsRiakContent a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakNamespace 'Nothing -- ^ Bucket type and bucket
  -> a -- ^ Object
  -> PutRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError (RiakContent a))
putNewRiakObjectBody
    handle namespace content (PutRiakObjectParams a b c d e f g h) =
  (fmap.fmap) List1.head $
    _putRiakObject handle namespace Nothing content a b c d e
      ParamObjectReturnBody f g h

_putRiakObject
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
_putRiakObject
    h@(RiakHandle manager cache) namespace@(RiakNamespace type' bucket) key
    value dw indexes metadata n pw return sloppy_quorum timeout
    w = liftIO . runExceptT $ do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe RiakVclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . riakCacheLookup cache . RiakLocation namespace)
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
        (\conn -> putRiakObjectPB conn request))

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


--------------------------------------------------------------------------------
-- Delete object
--------------------------------------------------------------------------------

-- | Delete an object or data type.
deleteRiakObject
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ty -- ^ Bucket type, bucket, and key
  -> m (Either RiakError ())
deleteRiakObject
    (RiakHandle manager cache)
    loc@(RiakLocation (RiakNamespace type' bucket) key) = liftIO $ do

  vclock :: Maybe RiakVclock <-
    riakCacheLookup cache loc

  let
    request :: RpbDelReq
    request =
      RpbDelReq
        { _RpbDelReq'_unknownFields = []
        , _RpbDelReq'bucket         = unRiakBucket bucket
        , _RpbDelReq'dw             = Nothing
        , _RpbDelReq'key            = unRiakKey key
        , _RpbDelReq'nVal           = Nothing
        , _RpbDelReq'pr             = Nothing
        , _RpbDelReq'pw             = Nothing
        , _RpbDelReq'r              = Nothing
        , _RpbDelReq'rw             = Nothing
        , _RpbDelReq'sloppyQuorum   = Nothing
        , _RpbDelReq'timeout        = Nothing
        , _RpbDelReq'type'          = Just (unRiakBucketType type')
        , _RpbDelReq'vclock         = coerce vclock
        , _RpbDelReq'w              = Nothing
        }

  fmap (() <$)
    (withRiakConnection manager (\conn -> deleteRiakObjectPB conn request))


--------------------------------------------------------------------------------
-- Get counter
--------------------------------------------------------------------------------

-- | Get a counter.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
getRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just 'RiakCounterTy) -- ^ Bucket type, bucket, and key
  -> GetRiakObjectParams -- ^ Optional parameters
  -> m (Either RiakError Int64)
getRiakCounter h loc (GetRiakObjectParams a b c d e f g) =
  getCrdt h loc (GetRiakCrdtParams a (IncludeContext Nothing) b c d e f g)


--------------------------------------------------------------------------------
-- Update counter
--------------------------------------------------------------------------------

-- | Update a counter and its updated value if @return_body@ is set, else 0.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
updateRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just 'RiakCounterTy) -- ^ Bucket type, bucket, and key
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError Int64)
updateRiakCounter h (RiakLocation namespace key) incr params =
  (fmap.fmap) snd (_updateRiakCounter h namespace (Just key) incr params)


-- | Update a new counter and return its randomly-generated key.
--
-- Throws:
--
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain counters.
updateNewRiakCounter
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakNamespace ('Just 'RiakCounterTy) -- ^
  -> Int64 -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError RiakKey)
updateNewRiakCounter h namespace incr params =
  (fmap.fmap) fst (_updateRiakCounter h namespace Nothing incr params)

_updateRiakCounter
  :: MonadIO m
  => RiakHandle
  -> RiakNamespace ('Just 'RiakCounterTy)
  -> Maybe RiakKey
  -> Int64
  -> UpdateRiakCrdtParams
  -> m (Either RiakError (RiakKey, Int64))
_updateRiakCounter h namespace key incr params =
  (fmap.fmap.fmap) (view #counterValue)
    (updateCrdt h namespace key Nothing op params)
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
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain grow-only
--   sets.
getRiakGrowOnlySet
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just 'RiakGrowOnlySetTy) -- ^ Bucket type, bucket, and key
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
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain HyperLogLogs.
getRiakHyperLogLog
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just 'RiakHyperLogLogTy) -- ^ Bucket type, bucket, and key
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
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain maps.
--
-- * 'RiakMapParseError' if decoding fails.
getRiakMap
  :: (IsRiakMap a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just ('RiakMapTy a)) -- ^ Bucket type, bucket, and key
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
-- * 'RiakCrdtError' if the given 'RiakLocation' does not contain sets.
--
-- * 'SomeException' if decoding fails. The exception thrown is provided by the
--   implementation of 'decodeRiakRegister'.
getRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just ('RiakSetTy a)) -- ^ Bucket type, bucket, and key
  -> GetRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (Set a))
getRiakSet =
  getCrdt


-------------------------------------------------------------------------------
-- Update set
--------------------------------------------------------------------------------

-- | Update a set and return its updated value if @return_body@ is set, else
-- the empty set.
--
-- Throws a 'RiakCrdtError' if the given 'RiakLocation' does not contain
-- sets.
updateRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakLocation ('Just ('RiakSetTy a)) -- ^ Bucket type, bucket, and key
  -> RiakSetOp a -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError (Set a))
updateRiakSet h (RiakLocation namespace key) op params =
  (fmap.fmap) snd (_updateSet h namespace (Just key) op params)

-- | Update a new set and return its randomly-generated key.
--
-- Throws a 'RiakCrdtError' if the given 'RiakLocation' does not contain
-- counters.
updateNewRiakSet
  :: (IsRiakSet a, MonadIO m)
  => RiakHandle -- ^ Riak handle
  -> RiakNamespace ('Just ('RiakSetTy a)) -- ^
  -> RiakSetOp a -- ^
  -> UpdateRiakCrdtParams -- ^ Optional parameters
  -> m (Either RiakError RiakKey)
updateNewRiakSet h namespace op params =
  (fmap.fmap) fst (_updateSet h namespace Nothing op params)

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
          loc :: RiakLocation ('Just ('RiakSetTy a))
          loc =
            RiakLocation namespace key'
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

  (key', value) <-
    ExceptT (updateCrdt h namespace key context op params)

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


--------------------------------------------------------------------------------
-- Get data type
--------------------------------------------------------------------------------

getCrdt
  :: forall m ty.
     (IsRiakCrdt ty, MonadIO m)
  => RiakHandle
  -> RiakLocation ('Just ty)
  -> GetRiakCrdtParams
  -> m (Either RiakError (CrdtVal ty))
getCrdt
    h@(RiakHandle manager _) loc@(RiakLocation (RiakNamespace type' bucket) key)
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


-------------------------------------------------------------------------------
-- Update data type
--------------------------------------------------------------------------------

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
        (\conn -> updateRiakCrdtPB conn request))

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
  -> RiakNamespace ty -- ^ Bucket type and bucket
  -> m (Either RiakError RpbBucketProps)
getRiakBucketProps (RiakHandle manager _) (RiakNamespace type' bucket) = liftIO . runExceptT $ do
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
      , _RpbGetBucketReq'bucket         = unRiakBucket bucket
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
  :: RiakHandle -- ^ Riak handle
  -> RiakBucketType ty -- ^ Bucket type
  -> (ListT (ExceptT RiakError IO) RiakBucket -> IO r)
  -> IO r
streamRiakBuckets (RiakHandle manager _) type' k =
  withRiakConnection manager $ \conn -> k $ do
    response :: RpbListBucketsResp <-
      streamRiakBucketsPB conn request

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

-- | List all of the buckets in a bucket type. Provided for convenience when
-- bringing all buckets into memory is okay.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
listRiakBuckets
  :: RiakHandle -- ^ Riak handle
  -> RiakBucketType ty -- ^ Bucket type
  -> IO (Either RiakError [RiakBucket])
listRiakBuckets h namespace =
  streamRiakBuckets h namespace
    (runExceptT . ListT.fold (\x a -> x . (a:)) id ($ []))

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
  -> RiakNamespace ty -- ^ Bucket type and bucket
  -> (ListT (ExceptT RiakError IO) RiakKey -> IO r)
  -> IO r
streamRiakKeys (RiakHandle manager _) (RiakNamespace type' bucket) k =
  withRiakConnection manager $ \conn -> k $ do
    response :: RpbListKeysResp <-
      streamRiakKeysPB conn request

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

-- | List all of the keys in a bucket. Provided for convenience when bringing
-- all keys into memory is okay.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
listRiakKeys
  :: RiakHandle -- ^ Riak handle
  -> RiakNamespace ty -- ^ Bucket type and bucket
  -> IO (Either RiakError [RiakKey])
listRiakKeys h namespace =
  streamRiakKeys h namespace
    (runExceptT . ListT.fold (\x a -> x . (a:)) id ($ []))


--------------------------------------------------------------------------------
-- MapReduce
--------------------------------------------------------------------------------

riakMapReduce
  :: RiakHandle -- ^ Riak handle
  -> RpbMapRedReq -- ^
  -> (ListT (ExceptT RiakError IO) RpbMapRedResp -> IO r)
  -> IO r
riakMapReduce (RiakHandle manager _) request k =
  withRiakConnection manager $ \conn -> k $
    riakMapReducePB conn request


--------------------------------------------------------------------------------
-- Search 2.0
--------------------------------------------------------------------------------

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
  -> RiakSchemaName -- ^ Schema name
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
      , _RpbYokozunaSchemaGetReq'name           = unRiakSchemaName schema
      }


putRiakSchema
  :: MonadIO m
  => RiakHandle -- ^ Riak handle
  -> RiakSchemaName -- ^ Schema name
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
            , _RpbYokozunaSchema'name           = unRiakSchemaName name
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
  -> RiakSchemaName -- ^
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
             , _RpbYokozunaIndex'schema         = Just (unRiakSchemaName schema)
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
  -> RiakLocation ty
  -> Maybe RiakVclock
  -> m ()
cacheVclock (RiakHandle _ cache) loc =
  liftIO . maybe (riakCacheDelete cache loc) (riakCacheInsert cache loc)


notTombstone :: RpbContent -> Bool
notTombstone content =
  not (content ^. #deleted)



parseContent
  :: forall a head.
     IsRiakContent a
  => Proxy# a
  -> RiakLocation 'Nothing
  -> SBool head
  -> RpbContent
  -> IO (RiakContent (If head () a))
parseContent _ loc head
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  theValue :: If head () a <-
    case head of
      STrue ->
        pure ()

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

-- TODO replace parseContent with parseContent', parseContentHead

parseContent'
  :: forall a.
     IsRiakContent a
  => RiakLocation 'Nothing
  -> RpbContent
  -> IO (RiakContent a)
parseContent' =
  _parseContent decodeRiakContent

parseContentHead
  :: forall a.
     IsRiakContent a
  => Proxy# a
  -> RiakLocation 'Nothing
  -> RpbContent
  -> IO (RiakContent ())
parseContentHead _ =
  _parseContent (\_ _ _ _ -> Right ())


_parseContent
  :: forall a.
     (  Maybe ContentType
     -> Maybe Charset
     -> Maybe ContentEncoding
     -> ByteString
     -> Either SomeException a
     )
  -> RiakLocation 'Nothing
  -> RpbContent
  -> IO (RiakContent a)
_parseContent parse loc
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) = do

  theValue :: a <-
    either throwIO pure
      (parse
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
