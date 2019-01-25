{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module ZZZ.Riak where
  -- ( -- * Handle
  --   createRiakHandle
  --   -- * Counter operations
  --   -- ** Get counter
  -- , getRiakCounter
  --   -- ** Update counter
  -- , updateRiakCounter
  -- , updateNewRiakCounter
  --   -- * Grow-only set operations
  --   -- ** Get grow-only set
  -- , getRiakGrowOnlySet
  --   -- ** Update grow-only set
  --   -- * HyperLogLog operations
  --   -- ** Get HyperLogLog
  -- , getRiakHyperLogLog
  --   -- ** Update HyperLogLog
  --   -- * Set operations
  --   -- ** Get set
  -- , getRiakSet
  --   -- ** Update set
  -- , updateRiakSet
  -- , updateNewRiakSet
  -- , riakSetAddOp
  -- , riakSetRemoveOp
  --   -- * Map operations
  -- , getRiakMap
  --   -- ** Get map
  -- , riakCounterField
  -- , riakFlagField
  -- , riakMapField
  -- , riakRegisterField
  -- , riakSetField
  --   -- ** Update map
  -- -- , allowExtraKeys
  --   -- * Bucket operations
  -- , getRiakBucketTypeProps
  -- , setRiakBucketTypeProps
  -- , getRiakBucketProps
  -- , setRiakBucketProps
  -- , resetRiakBucketProps
  --   -- * Full key traversals
  -- , streamRiakBuckets
  -- , streamRiakKeys
  --   -- * MapReduce
  --   -- $mapreduce
  -- , riakMapReduceBucket
  -- , riakMapReduceKeys
  -- , riakMapReduceFunction
  -- , riakMapReduceExactQuery
  -- , riakMapReduceRangeQuery
  -- , riakMapReducePhaseMapIdentity
  -- , riakMapReducePhaseMapObjectValue
  -- , riakMapReducePhaseReduceCount
  -- , riakMapReducePhaseReduceSetUnion
  -- , riakMapReducePhaseReduceSort
  -- , riakMapReducePhaseReduceSum
  --   -- * Secondary indexes (2i)
  --   -- $secondary-indexes
  -- , riakExactQuery
  -- , riakRangeQuery
  -- , riakRangeQueryTerms
  --   -- * Search 2.0
  --   -- ** Search
  -- , riakSearch
  --   -- ** Schema
  -- , getRiakSchema
  -- , putRiakSchema
  --   -- ** Index
  -- , getRiakIndex
  -- , getRiakIndexes
  -- , putRiakIndex
  -- , deleteRiakIndex
  --   -- * Server info
  -- , pingRiak
  -- , getRiakServerInfo
  --   -- * Types
  -- , IsRiakMap(..)
  -- , IsRiakRegister(..)
  -- , IsRiakSet
  -- , Modified(..)
  -- , RiakBucket(..)
  -- , pattern DefaultRiakBucket
  -- , RiakBucketType(..)
  -- , pattern DefaultRiakBucketType
  -- , RiakCrdtError(..)
  -- , RiakError(..)
  -- , RiakExactQuery(..)
  -- , RiakHandle
  -- , RiakIndex(..)
  -- , RiakIndexName(..)
  -- , RiakKey(..)
  -- , RiakMapEntries(..)
  -- , RiakMapFieldParser
  -- , RiakMapParseError(..)
  -- , RiakMapReduceInputs(..)
  -- , RiakMapReducePhase(..)
  -- , RiakMetadata(..)
  -- , RiakQuorum(..)
  -- , pattern RiakQuorumAll
  -- , pattern RiakQuorumQuorum
  -- , RiakRangeQuery(..)
  -- , RiakSetOp
  -- , RiakVtag(..)
  -- , RpbMapRedResp
  -- , SolrIndexName(..)
  -- , pattern DontIndex
  -- , SolrSchemaName(..)
  -- , pattern DefaultSolrSchemaName
  -- , TTL(..)
  --   -- * Optional parameters
  -- , GetRiakCrdtParams
  -- , GetRiakObjectParams
  -- , PutRiakObjectParams
  -- , RiakSearchParams
  -- , UpdateRiakCrdtParams
  --   -- * Re-exports
  -- , def
  -- , HostName
  -- , PortNumber
  --   -- * Documentation
  --   -- $documentation
  -- ) where

-- import Riak.Interface              (Interface, Result(..))
-- import Riak.Internal.Panic
-- import Riak.Internal.Prelude
-- import Riak.Internal.Utils         (bs2int, int2bs)
-- import Riak.Proto
-- import ZZZ.Riak.Internal.Cache
-- import ZZZ.Riak.Internal.Crdts
-- import ZZZ.Riak.Internal.Manager
-- import ZZZ.Riak.Internal.MapReduce
-- import ZZZ.Riak.Internal.Params
-- -- import ZZZ.Riak.Internal.Types     (RiakTy(..))
-- import ZZZ.Riak.Internal.Types

-- import qualified Riak.Interface  as Interface
-- import qualified Riak.Proto.Lens as L

-- import Control.Foldl      (FoldM)
-- import Control.Lens       (view)
-- import Data.Default.Class (def)
-- import Data.Profunctor    (lmap)
-- import Network.Socket     (HostName, PortNumber)

-- import qualified Control.Foldl as Foldl
-- import qualified Lens.Labels   as L


-- TODO _ variants that don't decode replies

-- TODO since annotations

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | A thread-safe handle to Riak.
--
-- TODO: RiakHandle optional cache
-- data RiakHandle
--   = RiakHandle !RiakManager !RiakCache

-- | Create a handle to Riak. When the handle is garbage collected, all
-- outstanding sockets (if any) are closed.
--
-- The following default settings are used, which can be overridden by using
-- functionality exposed in the "Riak.Internal" module:
--
-- * Object and data type causal contexts are cached using an in-memory
--   map from @stm-containers@.
--
-- * A maximum of two sockets per capability are opened.
--
-- * Inactive sockets are closed after 30 seconds.
--createRiakHandle
--  :: MonadIO m
--  => HostName -- ^ Host
--  -> PortNumber -- ^ Port
--  -> m RiakHandle
--createRiakHandle host port = liftIO $ do
--  cache :: RiakCache <-
--    newSTMRiakCache

--  sockets :: Int <-
--    (* 2) <$> getNumCapabilities

--  manager :: RiakManager <-
--    createRiakManager host port sockets 30

--  pure (RiakHandle manager cache)


----------------------------------------------------------------------------------
---- Get counter
----------------------------------------------------------------------------------

---- TODO: make updates throw RiakCrdtError as well

---- | Get a counter.
----
---- Throws:
----
---- * 'RiakCrdtError' if the given 'RiakKey' does not contain counters.
--getRiakCounter
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> GetRiakObjectParams -- ^ Optional parameters
--  -> m (Result (Maybe Int64))
--getRiakCounter h key (GetRiakObjectParams a b c d e f g) =
--  getCrdt (proxy# @_ @'RiakCounterTy) h key
--    (GetRiakCrdtParams a (IncludeContext Nothing) b c d e f g)


----------------------------------------------------------------------------------
---- Update counter
----------------------------------------------------------------------------------

---- | Update a counter and its updated value if @return_body@ is set, else 0.
--updateRiakCounter
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> Int64 -- ^
--  -> UpdateRiakCrdtParams -- ^ Optional parameters
--  -> m (Result Int64)
--updateRiakCounter h (RiakKey bucket key) incr params =
--  (fmap.fmap) snd (_updateRiakCounter h bucket (Just key) incr params)


---- | Update a new counter and return its randomly-generated key.
--updateNewRiakCounter
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> Int64 -- ^
--  -> UpdateRiakCrdtParams -- ^ Optional parameters
--  -> m (Result RiakKey)
--updateNewRiakCounter h bucket incr params =
--  (fmap.fmap) fst (_updateRiakCounter h bucket Nothing incr params)

--_updateRiakCounter
--  :: MonadIO m
--  => RiakHandle
--  -> RiakBucket
--  -> Maybe ByteString
--  -> Int64
--  -> UpdateRiakCrdtParams
--  -> m (Result (RiakKey, Int64))
--_updateRiakCounter h bucket key incr params =
--  (fmap.fmap.fmap) (L.view #counterValue)
--    (updateCrdt h bucket key Nothing op params)
--  where
--    op :: DtOp
--    op =
--      defMessage
--        & #counterOp L..~ (defMessage & #increment L..~ incr)


----------------------------------------------------------------------------------
---- Get grow-only set
----------------------------------------------------------------------------------

---- | Get a grow-only set.
----
---- Throws:
----
---- * 'RiakCrdtError' if the given 'RiakKey' does not contain grow-only
----   sets.
--getRiakGrowOnlySet
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> GetRiakCrdtParams -- ^ Optional parameters
--  -> m (Result (Maybe (Set ByteString)))
--getRiakGrowOnlySet =
--  getCrdt (proxy# @_ @'RiakGrowOnlySetTy)


----------------------------------------------------------------------------------
---- Update grow-only set
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
---- Get HyperLogLog
----------------------------------------------------------------------------------

---- | Get a HyperLogLog.
----
---- Throws
----
---- * 'RiakCrdtError' if the given 'RiakKey' does not contain HyperLogLogs.
--getRiakHyperLogLog
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> GetRiakCrdtParams -- ^ Optional parameters
--  -> m (Result (Maybe Word64))
--getRiakHyperLogLog =
--  getCrdt (proxy# @_ @'RiakHyperLogLogTy)


----------------------------------------------------------------------------------
---- Update HyperLogLog
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
---- Get map
----------------------------------------------------------------------------------

---- | Get and decode a map.
----
---- Throws:
----
---- * 'RiakCrdtError' if the given 'RiakKey' does not contain maps.
----
---- * 'RiakMapParseError' if decoding fails.
--getRiakMap
--  :: forall a m.
--     (IsRiakMap a, MonadIO m)
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> GetRiakCrdtParams -- ^ Optional parameters
--  -> m (Result (Maybe a))
--getRiakMap =
--  getCrdt (proxy# @_ @('RiakMapTy a))


---------------------------------------------------------------------------------
---- Update map
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
---- Get set
----------------------------------------------------------------------------------

---- | Get and decode a set.
----
---- Throws:
----
---- * 'RiakCrdtError' if the given 'RiakKey' does not contain sets.
----
---- * 'SomeException' if decoding fails. The exception thrown is provided by the
----   implementation of 'decodeRiakRegister'.
--getRiakSet
--  :: forall a m.
--     (IsRiakSet a, MonadIO m)
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> GetRiakCrdtParams -- ^ Optional parameters
--  -> m (Result (Maybe (Set a)))
--getRiakSet =
--  getCrdt (proxy# @_ @('RiakSetTy a))


---------------------------------------------------------------------------------
---- Update set
----------------------------------------------------------------------------------

---- | Update a set and return its updated value if @return_body@ is set, else
---- the empty set.
--updateRiakSet
--  :: (IsRiakSet a, MonadIO m)
--  => RiakHandle -- ^ Riak handle
--  -> RiakKey -- ^ Bucket type, bucket, and key
--  -> RiakSetOp a -- ^
--  -> UpdateRiakCrdtParams -- ^ Optional parameters
--  -> m (Result (Set a))
--updateRiakSet h (RiakKey bucket key) op params =
--  (fmap.fmap) snd (_updateSet h bucket (Just key) op params)

---- | Update a new set and return its randomly-generated key.
--updateNewRiakSet
--  :: (IsRiakSet a, MonadIO m)
--  => RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> RiakSetOp a -- ^ Set operation
--  -> UpdateRiakCrdtParams -- ^ Optional parameters
--  -> m (Result RiakKey)
--updateNewRiakSet h bucket op params =
--  (fmap.fmap) fst (_updateSet h bucket Nothing op params)

---- TODO _updateSet use same conn for get-put
--_updateSet
--  :: forall a m.
--     (IsRiakSet a, MonadIO m)
--  => RiakHandle
--  -> RiakBucket
--  -> Maybe ByteString
--  -> RiakSetOp a
--  -> UpdateRiakCrdtParams
--  -> m (Result (RiakKey, (Set a)))
--_updateSet h namespace key setOp params = liftIO $ do
--  context :: Maybe RiakVclock <-
--    _updateSetGetContext @a h namespace key removes

--  updateCrdt h namespace key context op params >>= \case
--    RiakClosedConnection ->
--      pure RiakClosedConnection

--    Failure err ->
--      pure (Failure err)

--    Success (loc, response) ->
--      case parseDtUpdateResp (proxy# @_ @('RiakSetTy a)) loc response of
--        Left err ->
--          throwIO err

--        Right value -> do
--          -- If the context was fetched, cache it
--          for_ (response L.^. #maybe'context) $ \vclock ->
--            cacheVclock h loc (Just (RiakVclock vclock))

--          pure (Success (loc, value))

--  where
--    op :: DtOp
--    op =
--      defMessage
--        & #setOp L..~
--            (defMessage
--              & #adds L..~ adds
--              & #removes L..~ removes)

--    (adds, removes) =
--      unSetOp setOp

---- Be a good citizen and perhaps include the cached causal context with this
---- update request.
----
---- (1) If the set definitely doesn't already exist, no context is required.
----
---- (2) If the set might exist but we aren't attempting any removes, no
----     context is required.
----
---- (3) If the set might exist and we are attempting at least one remove, a
----     context is required (more like "recommended").
----
----     (a) If we have one cached, hooray.
----
----     (b) If we don't have one cached, perform a quick get first. Note that
----         we won't get a context if the set doesn't exist - this would mean
----         we are attempting to remove something from an empty set. That is
----         weird, so I'm okay with not passing in a context in this situation
----         (we don't have one to pass, anyway).
--_updateSetGetContext ::
--     forall a.
--     IsRiakSet a
--  => RiakHandle
--  -> RiakBucket
--  -> Maybe ByteString
--  -> [ByteString]
--  -> IO (Maybe RiakVclock)
--_updateSetGetContext h@(RiakHandle _ cache) namespace key removes = do
--  case key of
--    -- (1)
--    Nothing ->
--      pure Nothing

--    Just key' ->
--      let
--        loc :: RiakKey
--        loc =
--          RiakKey namespace key'
--      in do
--        context :: Maybe RiakVclock <-
--          riakCacheLookup cache loc

--        if null removes
--          then
--            -- (2)
--            pure context

--          else
--            case context of
--              -- (3b)
--              Nothing -> do
--                _ <- getRiakSet @a h loc def
--                riakCacheLookup cache loc

--              -- (3a)
--              Just context' ->
--                pure (Just context')



----------------------------------------------------------------------------------
---- Get data type
----------------------------------------------------------------------------------

--getCrdt
--  :: forall m ty.
--     (IsRiakCrdt ty, MonadIO m)
--  => Proxy# ty
--  -> RiakHandle
--  -> RiakKey
--  -> GetRiakCrdtParams
--  -> m (Result (Maybe (CrdtVal ty)))
--getCrdt
--    p h@(RiakHandle manager _) loc@(RiakKey (RiakBucket type' bucket) key)
--    (GetRiakCrdtParams basic_quorum (IncludeContext include_context) n
--      notfound_ok pr r sloppy_quorum timeout) = liftIO $ do

--  withRiakConnection manager (\conn -> Interface.getCrdt conn request) >>= \case
--    RiakClosedConnection ->
--      pure RiakClosedConnection

--    Failure err ->
--      pure (Failure err)

--    Success response ->
--      case parseDtFetchResp p loc response of
--        Left err ->
--          throwIO err

--        Right value -> do
--          -- Only counters don't have a causal context
--          let
--            shouldCache :: Bool
--            shouldCache =
--              response L.^. #type' /= DtFetchResp'COUNTER &&
--                include_context /= Just False

--          when shouldCache $
--            cacheVclock
--              h
--              loc
--              (coerce (response L.^. #maybe'context))

--          pure (Success value)

--  where
--    request :: DtFetchReq
--    request =
--      defMessage
--        & #maybe'basicQuorum L..~ unBasicQuorum basic_quorum
--        & #bucket L..~ bucket
--        & #maybe'includeContext L..~ include_context
--        & #key L..~ key
--        & #maybe'nVal L..~ coerce n
--        & #maybe'notfoundOk L..~ unNotfoundOk notfound_ok
--        & #maybe'pr L..~ coerce pr
--        & #maybe'r L..~ coerce r
--        & #maybe'sloppyQuorum L..~ coerce sloppy_quorum
--        & #maybe'timeout L..~ coerce timeout
--        & #type' L..~ unRiakBucketType type'


---------------------------------------------------------------------------------
---- Update data type
----------------------------------------------------------------------------------

--updateCrdt
--  :: MonadIO m
--  => RiakHandle
--  -> RiakBucket
--  -> Maybe ByteString
--  -> Maybe RiakVclock
--  -> DtOp
--  -> UpdateRiakCrdtParams
--  -> m (Result (RiakKey, DtUpdateResp))
--updateCrdt
--    (RiakHandle manager _) namespace@(RiakBucket type' bucket) key context op
--    (UpdateRiakCrdtParams dw n pw return_body sloppy_quorum timeout w) =
--    liftIO $ do

--  withRiakConnection manager (\conn -> Interface.updateCrdt conn request) >>= \case
--    RiakClosedConnection ->
--      pure RiakClosedConnection

--    Failure err ->
--      pure (Failure err)

--    Success response -> do
--      theKey :: ByteString <-
--        maybe
--          (maybe
--            (panic "missing key"
--              ( ( "request" , request  )
--              , ( "response", response )
--              ))
--            pure
--            (response L.^. #maybe'key))
--          pure
--          key

--      pure (Success (RiakKey namespace theKey, response))

--  where
--    request :: DtUpdateReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #maybe'context L..~ coerce context
--        & #maybe'dw L..~ coerce dw
--        & #maybe'key L..~ coerce key
--        & #maybe'nVal L..~ coerce n
--        & #op L..~ op
--        & #maybe'pw L..~ coerce pw
--        & #maybe'returnBody L..~ unReturnBody return_body
--        & #maybe'sloppyQuorum L..~ unSloppyQuorum sloppy_quorum
--        & #maybe'timeout L..~ unTimeout timeout
--        & #type' L..~ unRiakBucketType type'
--        & #maybe'w L..~ coerce w


----------------------------------------------------------------------------------
---- Get bucket type properties
----------------------------------------------------------------------------------

---- TODO getRiakBucketTypeProps return BucketProps
--getRiakBucketTypeProps
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakBucketType -- ^ Bucket type
--  -> m (Result RpbBucketProps)
--getRiakBucketTypeProps (RiakHandle manager _) type' = liftIO $ do
--  fmap (^. L.props) <$>
--    withRiakConnection manager
--      (\conn -> Interface.getBucketTypeProps conn request)

--  where
--    request :: RpbGetBucketTypeReq
--    request =
--      defMessage
--        & #type' L..~ unRiakBucketType type'


----------------------------------------------------------------------------------
---- Set bucket type properties
----------------------------------------------------------------------------------

---- TODO: Don't allow setting n
--setRiakBucketTypeProps
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakBucketType -- ^ Bucket type
--  -> RpbBucketProps -- ^
--  -> m (Result ())
--setRiakBucketTypeProps (RiakHandle manager _) type' props =
--  liftIO (fmap (() <$)
--    (withRiakConnection manager
--      (\conn -> Interface.setBucketTypeProps conn request)))
--  where
--    request :: RpbSetBucketTypeReq
--    request =
--      defMessage
--        & #props L..~ props
--        & #type' L..~ unRiakBucketType type'


----------------------------------------------------------------------------------
---- Get bucket props
----------------------------------------------------------------------------------

---- TODO getRiakBucketProps return BucketProps
--getRiakBucketProps
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> m (Result RpbBucketProps)
--getRiakBucketProps (RiakHandle manager _) (RiakBucket type' bucket) = liftIO $ do
--  fmap (^. L.props) <$>
--    withRiakConnection manager
--      (\conn -> Interface.getBucketProps conn request)

--  where
--    request :: RpbGetBucketReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #type' L..~ unRiakBucketType type'


----------------------------------------------------------------------------------
---- Set bucket props
----------------------------------------------------------------------------------

---- TODO: setRiakBucketProps Don't allow setting n
--setRiakBucketProps
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RpbSetBucketReq -- ^
--  -> m (Result ())
--setRiakBucketProps (RiakHandle manager _) req =
--  liftIO (fmap (() <$)
--    (withRiakConnection manager
--      (\conn -> Interface.setBucketProps conn req)))


----------------------------------------------------------------------------------
---- Reset bucket props
----------------------------------------------------------------------------------

--resetRiakBucketProps
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RpbResetBucketReq -- ^
--  -> m (Result ())
--resetRiakBucketProps (RiakHandle manager _) req = liftIO $
--  fmap (() <$)
--    (withRiakConnection manager
--      (\conn -> Interface.resetBucketProps conn req))


----------------------------------------------------------------------------------
---- List buckets
----------------------------------------------------------------------------------

---- | Stream all of the buckets in a bucket type.
----
---- /Note/: This is an extremely expensive operation, and should not be used on a
---- production cluster.
----
---- TODO streamRiakBuckets param timeout
--streamRiakBuckets
--  :: forall r.
--     RiakHandle -- ^ Riak handle
--  -> RiakBucketType -- ^ Bucket type
--  -> FoldM IO RiakBucket r -- ^ Bucket fold
--  -> IO (Result r)
--streamRiakBuckets (RiakHandle manager _) type' fold =
--  withRiakConnection manager $ \conn -> do
--    Interface.streamBuckets conn request
--      (fold
--        & Foldl.handlesM Foldl.folded
--        & lmap (map (RiakBucket type') . (L.^. #buckets)))
--  where
--    request :: RpbListBucketsReq
--    request =
--      defMessage
--        & #stream L..~ True
--        & #type' L..~ unRiakBucketType type'


----------------------------------------------------------------------------------
---- List keys
----------------------------------------------------------------------------------

---- | Stream all of the keys in a bucket.
----
---- /Note/: This is an extremely expensive operation, and should not be used on a
---- production cluster.
----
---- TODO streamRiakKeys param timeout
--streamRiakKeys
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> FoldM IO RiakKey r -- ^ Key fold
--  -> IO (Result r)
--streamRiakKeys (RiakHandle manager _) namespace@(RiakBucket type' bucket) fold =
--  withRiakConnection manager $ \conn -> do
--    Interface.streamKeys conn request
--      (fold
--        & Foldl.handlesM Foldl.folded
--        & lmap (map (RiakKey namespace) . (L.^. #keys)))

--  where
--    request :: RpbListKeysReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #type' L..~ unRiakBucketType type'


----------------------------------------------------------------------------------
---- MapReduce
----------------------------------------------------------------------------------

---- $mapreduce
----
---- * <http://docs.basho.com/riak/kv/2.2.3/developing/usage/mapreduce/>
----
---- * <http://docs.basho.com/riak/kv/2.2.3/developing/app-guide/advanced-mapreduce/>
----
---- * <http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/mapreduce/>

--riakMapReduceBucket
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--riakMapReduceBucket h bucket =
--  _riakMapReduce h (RiakMapReduceInputsBucket bucket)

--riakMapReduceKeys
--  :: RiakHandle -- ^ Riak handle
--  -> [RiakKey] -- ^ Bucket types, buckets, and keys
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--riakMapReduceKeys h keys =
--  _riakMapReduce h (RiakMapReduceInputsKeys keys)

--riakMapReduceFunction
--  :: RiakHandle -- ^ Riak handle
--  -> Text -- ^ Erlang module.
--  -> Text -- ^ Erlang function.
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--riakMapReduceFunction h m f =
--  _riakMapReduce h (RiakMapReduceInputsFunction m f)

--riakMapReduceExactQuery
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket.
--  -> RiakExactQuery -- ^ Exact query.
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--riakMapReduceExactQuery h bucket query =
--  _riakMapReduce h (RiakMapReduceInputsExactQuery bucket query)

--riakMapReduceRangeQuery
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket.
--  -> RiakRangeQuery a -- ^ Range query.
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--riakMapReduceRangeQuery h bucket query =
--  _riakMapReduce h (RiakMapReduceInputsRangeQuery bucket query)

--_riakMapReduce
--  :: RiakHandle -- ^ Riak handle
--  -> RiakMapReduceInputs -- ^ MapReduce inputs
--  -> [RiakMapReducePhase] -- ^ MapReduce phases
--  -> FoldM IO RpbMapRedResp r -- ^
--  -> IO (Result r)
--_riakMapReduce (RiakHandle manager _) inputs query k =
--  withRiakConnection manager $ \conn ->
--    Interface.mapReduce conn (riakMapReduceRequest inputs query) k

----------------------------------------------------------------------------------
---- Secondary indexes
----------------------------------------------------------------------------------

---- $secondary-indexes
----
---- * <http://docs.basho.com/riak/kv/2.2.3/developing/usage/secondary-indexes/>
----
---- * <http://docs.basho.com/riak/kv/2.2.3/using/reference/secondary-indexes/>
----
---- * <http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/secondary-indexes/>

---- TODO 2i query paging

---- | Perform an exact query on a secondary index.
--riakExactQuery
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> RiakExactQuery -- ^ Exact query
--  -> FoldM IO RiakKey r -- ^ Key fold
--  -> IO (Result r)
--riakExactQuery
--    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
--  withRiakConnection manager $ \conn ->
--    Interface.index conn request
--      (fold
--        & Foldl.handlesM Foldl.folded
--        & lmap (map (RiakKey namespace) . (L.^. #keys)))
--  where
--    request :: RpbIndexReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #index L..~ unRiakIndexName index
--        & #key L..~ key
--        & #qtype L..~ RpbIndexReq'eq
--        & #stream L..~ True
--        & #type' L..~ unRiakBucketType type'

--    index :: RiakIndexName
--    index =
--      case query of
--        RiakExactQueryBin (RiakIndexName idx) _ ->
--          RiakIndexName (idx <> "_bin")

--        RiakExactQueryInt (RiakIndexName idx) _ ->
--          RiakIndexName (idx <> "_int")

--    key :: ByteString
--    key =
--      case query of
--        RiakExactQueryBin _ n -> n
--        RiakExactQueryInt _ n -> int2bs n


---- | Perform a range query on a secondary index.
--riakRangeQuery
--  :: RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> RiakRangeQuery a -- ^ Range query
--  -> FoldM IO RiakKey r -- ^ Key fold
--  -> IO (Result r)
--riakRangeQuery
--    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
--  withRiakConnection manager $ \conn ->
--    Interface.index conn request
--      (fold
--        & Foldl.handlesM Foldl.folded
--        & lmap (map (RiakKey namespace) . (L.^. #keys)))
--  where
--    request :: RpbIndexReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #index L..~ unRiakIndexName index
--        & #qtype L..~ RpbIndexReq'range
--        & #rangeMax L..~ rmax
--        & #rangeMin L..~ rmin
--        & #stream L..~ True
--        & #type' L..~ unRiakBucketType type'

--    index :: RiakIndexName
--    index =
--      case query of
--        RiakRangeQueryBin (RiakIndexName idx) _ _ ->
--          RiakIndexName (idx <> "_bin")
--        RiakRangeQueryInt (RiakIndexName idx) _ _ ->
--          RiakIndexName (idx <> "_int")

--    rmin :: ByteString
--    rmin =
--      case query of
--        RiakRangeQueryBin _ n _ -> n
--        RiakRangeQueryInt _ n _ -> int2bs n

--    rmax :: ByteString
--    rmax =
--      case query of
--        RiakRangeQueryBin _ _ n -> n
--        RiakRangeQueryInt _ _ n -> int2bs n

---- | Perform a range query on a secondary index, and return the indexed terms
---- along with the keys.
--riakRangeQueryTerms
--  :: forall a r.
--     RiakHandle -- ^ Riak handle
--  -> RiakBucket -- ^ Bucket type and bucket
--  -> RiakRangeQuery a -- ^ Range query
--  -> FoldM IO (RiakKey, a) r -- ^ Key+value fold
--  -> IO (Result r)
--riakRangeQueryTerms
--    (RiakHandle manager _) namespace@(RiakBucket type' bucket) query fold =
--  withRiakConnection manager $ \conn ->
--    Interface.index conn request
--      (fold
--        & Foldl.handlesM Foldl.folded
--        & lmap (map parse . (L.^. #results)))
--  where
--    request :: RpbIndexReq
--    request =
--      defMessage
--        & #bucket L..~ bucket
--        & #index L..~ unRiakIndexName index
--        & #qtype L..~ RpbIndexReq'range
--        & #rangeMax L..~ rmax
--        & #rangeMin L..~ rmin
--        & #returnTerms L..~ True
--        & #stream L..~ True
--        & #type' L..~ unRiakBucketType type'

--    index :: RiakIndexName
--    index =
--      case query of
--        RiakRangeQueryBin (RiakIndexName idx) _ _ ->
--          RiakIndexName (idx <> "_bin")
--        RiakRangeQueryInt (RiakIndexName idx) _ _ ->
--          RiakIndexName (idx <> "_int")

--    rmin :: ByteString
--    rmin =
--      case query of
--        RiakRangeQueryBin _ n _ -> n
--        RiakRangeQueryInt _ n _ -> int2bs n

--    rmax :: ByteString
--    rmax =
--      case query of
--        RiakRangeQueryBin _ _ n -> n
--        RiakRangeQueryInt _ _ n -> int2bs n

--    parse :: RpbPair -> (RiakKey, a)
--    parse =
--      unRpbPair >>> \case
--        (val, Just key) ->
--          (RiakKey namespace key, parse1 val)
--        _ ->
--          undefined

--    parse1 :: ByteString -> a
--    parse1 =
--      case query of
--        RiakRangeQueryBin _ _ _ -> id
--        RiakRangeQueryInt _ _ _ -> bs2int



----------------------------------------------------------------------------------
---- Search 2.0
----------------------------------------------------------------------------------

---- | Execute a search.
--riakSearch
--  :: RiakHandle -- ^ Riak handle
--  -> ByteString -- ^ Query
--  -> ByteString -- ^ Index
--  -> RiakSearchParams -- ^ Optional parameters
--  -> IO (Result RpbSearchQueryResp)
--riakSearch (RiakHandle manager _)
--    query index
--    (RiakSearchParams df filter' fl op presort rows sort start) =
--  withRiakConnection manager $ \conn ->
--    Interface.search conn request
--  where
--    request :: RpbSearchQueryReq
--    request =
--      defMessage
--        & #maybe'df L..~ unDF df
--        & #maybe'filter L..~ unFilter filter'
--        & #fl L..~ unFL fl
--        & #index L..~ index
--        & #maybe'op L..~ unOp op
--        & #maybe'presort L..~ unPresort presort
--        & #q L..~ query
--        & #maybe'rows L..~ unRows rows
--        & #maybe'sort L..~ unSort sort
--        & #maybe'start L..~ unStart start


--getRiakSchema
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> SolrSchemaName -- ^ Schema name
--  -> m (Result (Maybe RpbYokozunaSchemaGetResp))
--getRiakSchema (RiakHandle manager _) schema = liftIO $
--  withRiakConnection manager
--    (\conn ->
--      translateNotfound <$> Interface.getSchema conn request)
--  where
--    request :: RpbYokozunaSchemaGetReq
--    request =
--      defMessage
--        & #name L..~ unSolrSchemaName schema


--putRiakSchema
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> SolrSchemaName -- ^ Schema name
--  -> ByteString -- ^ Schema contents
--  -> m (Result ())
--putRiakSchema (RiakHandle manager _) name bytes = liftIO $
--  fmap (() <$)
--    (withRiakConnection manager
--      (\conn -> Interface.putSchema conn request))
--  where
--    request :: RpbYokozunaSchemaPutReq
--    request =
--      defMessage
--        & #schema L..~
--            (defMessage
--              & #content L..~ bytes
--              & #name L..~ unSolrSchemaName name)


--getRiakIndex
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakIndexName -- ^
--  -> m (Result (Maybe RpbYokozunaIndex))
--getRiakIndex (RiakHandle manager _) name = liftIO $ do
--  withRiakConnection manager action
--  where
--    request :: RpbYokozunaIndexGetReq
--    request =
--      defMessage
--        & #name L..~ unRiakIndexName name

--    action
--      :: Interface
--      -> IO (Result (Maybe RpbYokozunaIndex))
--    action conn =
--      (translateNotfound <$> Interface.getIndex conn request) >>= \case
--        RiakClosedConnection ->
--          pure (RiakClosedConnection)

--        Failure err ->
--          pure (Failure err)

--        Success Nothing ->
--          pure (Success Nothing)

--        Success (Just response) ->
--          case response ^. L.index of
--            [index] ->
--              pure (Success (Just index))

--            _ ->
--              panic "0 or 2+ indexes"
--                ( ( "request",  request  )
--                , ( "response", response )
--                )

--getRiakIndexes
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> m (Result [RpbYokozunaIndex])
--getRiakIndexes (RiakHandle manager _) = liftIO $
--  fmap (^. L.index) <$>
--    (withRiakConnection manager
--      (\conn -> Interface.getIndex conn defMessage))


--putRiakIndex
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RiakIndexName -- ^
--  -> SolrSchemaName -- ^
--  -> m (Result ())
--putRiakIndex (RiakHandle manager _) index schema = liftIO $
--  fmap (() <$)
--    (withRiakConnection manager
--      (\conn -> Interface.putIndex conn request))
--  where
--    request :: RpbYokozunaIndexPutReq
--    request =
--      defMessage
--        & #index L..~
--            (defMessage
--              & #maybe'nVal L..~ Nothing -- TODO putRiakIndex n_val
--              & #name L..~ unRiakIndexName index
--              & #schema L..~ unSolrSchemaName schema)
--        & #maybe'timeout L..~ Nothing -- TODO putRiakIndex timeout


--deleteRiakIndex
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> RpbYokozunaIndexDeleteReq -- ^
--  -> m (Result RpbDelResp)
--deleteRiakIndex (RiakHandle manager _) req = liftIO $
--  withRiakConnection manager
--    (\conn -> Interface.deleteIndex conn req)


----------------------------------------------------------------------------------
---- Server info
----------------------------------------------------------------------------------

--pingRiak
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> m (Result ())
--pingRiak (RiakHandle manager _) = liftIO $
--  fmap (() <$) (withRiakConnection manager Interface.ping)


--getRiakServerInfo
--  :: MonadIO m
--  => RiakHandle -- ^ Riak handle
--  -> m (Result RpbGetServerInfoResp)
--getRiakServerInfo (RiakHandle manager _) = liftIO $
--  withRiakConnection manager Interface.getServerInfo


----------------------------------------------------------------------------------
---- Misc.
----------------------------------------------------------------------------------

---- | Given a fetched vclock, update the cache (if present) or delete it from the
---- cache (if missing).
--cacheVclock
--  :: MonadIO m
--  => RiakHandle
--  -> RiakKey
--  -> Maybe RiakVclock
--  -> m ()
--cacheVclock (RiakHandle _ cache) loc =
--  liftIO . maybe (riakCacheDelete cache loc) (riakCacheInsert cache loc)

--translateNotfound :: Result a -> Result (Maybe a)
--translateNotfound = \case
--  RiakClosedConnection ->
--    RiakClosedConnection

--  Failure (view L.errmsg -> "notfound") ->
--    Success Nothing

--  Failure err ->
--    Failure err

--  Success result ->
--    Success (Just result)

--unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
--unRpbPair pair =
--  ( pair L.^. #key
--  , pair L.^. #maybe'value
--  )

--unSetOp :: IsRiakSet a => RiakSetOp a -> ([ByteString], [ByteString])
--unSetOp (RiakSetOp (adds, removes)) =
--  ( map encodeRiakRegister (toList adds)
--  , map encodeRiakRegister (toList removes)
--  )

---- $documentation
----
---- = Objects
----
---- TODO write this
----
---- = Optional parameters
----
---- Each request takes a bundle of optional parameters as a data type named
---- @*Params@. It is always constructed using 'def' and overloaded labels syntax.
----
---- For example, 'GetRiakObjectParams' has an instance
----
---- @
---- IsLabel "sloppy_quorum" (Bool -> GetRiakObjectParams -> GetRiakObjectParams)
---- @
----
---- To use this instance, write
----
---- @
---- def & #sloppy_quorum False
---- @
----
---- = @vclock@ cache
----
---- Riak objects carry a causal context (either a vclock or a dotted version
---- vector) that help Riak resolve some conflicts automatically.
----
---- Proper handling of the causal context is fairly simple: to perform a write,
---- first perform a read to fetch the causal context, then include it in the
---- write request. This will minimize, but not eliminate, the creation of
---- siblings.
----
---- This library caches every vclock fetched and includes them in write requests
---- automatically. Siblings are /not/ automatically resolved: if you
---- ever read siblings, it is your responsibility to resolve them in your
---- application and perform a followup write to eliminate them.
----
---- Riak data types, which cannot have siblings, also have causal contexts that
---- are cached and included in write requests automatically.
----
---- = Glossary
----
---- [__allow mult__]
---- Whether siblings can be created. The legacy default bucket type defaults to
---- false, but other bucket types default to true, and true is the recommended
---- setting for most use cases.
----
---- [__basic quorum__]
---- Whether to use the "basic quorum" policy for not-founds. Only relevant when
---- __notfound_ok__ is set to false.
----
----     * /Default/: false.
----
---- [__dw__]
---- The number of vnodes that must write a write request to storage before a
---- response is returned to the client. The request will still be replicated to
---- __n__ vnodes.
----
----     * /Default/: @quorum@.
----
----     * /Range/: 1 to __n__.
----
---- [__last write wins__]
---- Resolve conflicts with timestamps. Only relevant if __allow mult__ is false,
---- which is not recommended.
----
---- [__n__]
---- The number of /primary vnodes/ responsible for each key, i.e. the number of
---- /replicas/ stored in the cluster.
----
----     * /Default/: 3.
----
----     * /Range/: 1 to the number of nodes in the cluster.
----
---- [__notfound ok__]
---- Controls how Riak behaves during read requests when keys are not present. If
---- @true@, Riak will treat any @notfound@ as a positive assertion that the key
---- does not exist. If @false@, Riak will treat any @notfound@ as a failure
---- condition. The coordinating node will wait for enough vnodes to reply with
---- @notfound@ to know that it cannot satisfy the requested __r__.
----
----     * /Default/: true.
----
---- [__pr__]
---- The number of primary vnodes that must respond to a read request before a
---- response is returned to the client. The request will still be replicated to
---- __n__ vnodes.
----
----     * /Default/: 0.
----
----     * /Range/: 1 to __n__.
----
---- [__pw__]
---- The number of primary vnodes that must /respond/ to a write request before a
---- response is returned to the client. The request will still be replicated to
---- __n__ vnodes.
----
----     * /Default/: 0.
----
----     * /Range/: 1 to __n__.
----
---- [__r__]
---- The number of vnodes that must respond to a read request before a response is
---- returned to the client. The request will still be replicated to __n__ vnodes.
----
----     * /Default/: @quorum@.
----
----     * /Range/: 1 to __n__.
----
---- [__sloppy quorum__]
---- Whether failover vnodes are consulted if one or more primary vnodes fails.
----
---- * /Default/: true.
----
---- [__w__]
---- The number of vnodes that must /respond/ to a write request before a response
---- is returned to the client. The request will still be replicated to __n__
---- vnodes.
----
----     * /Default/: @quorum@.
----
----     * /Range/: 1 to __n__.
