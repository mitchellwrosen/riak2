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
