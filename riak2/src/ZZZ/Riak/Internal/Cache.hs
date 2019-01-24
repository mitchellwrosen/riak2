module ZZZ.Riak.Internal.Cache where
  -- ( RiakCache(..)
  -- , newSTMRiakCache
  -- ) where

-- import Control.Concurrent.STM

-- import qualified StmContainers.Map as STM (Map)
-- import qualified StmContainers.Map as STMMap

-- import Riak.Internal.Prelude
-- import ZZZ.Riak.Internal.Debug
-- import ZZZ.Riak.Internal.Types

-- -- TODO cache ttl

-- data RiakCache
--   = RiakCache
--   { riakCacheLookup :: RiakKey -> IO (Maybe RiakVclock)
--   , riakCacheInsert :: RiakKey -> RiakVclock -> IO ()
--   , riakCacheDelete :: RiakKey -> IO ()
--   }

-- -- | Create a 'RiakCache' backed by an STM map.
-- newSTMRiakCache :: IO RiakCache
-- newSTMRiakCache = do
--   cache :: STM.Map RiakKey RiakVclock <-
--     STMMap.newIO

--   pure RiakCache
--     { riakCacheLookup = do
--         \key -> do
--           vclock <- atomically (STMMap.lookup key cache)
--           debug $
--             "[riak] cache lookup: " ++ show vclock ++ " <= " ++
--             show key
--           pure vclock

--     , riakCacheInsert =
--         \key vclock -> do
--           debug $
--             "[riak] cache insert: " ++ show key ++ " => " ++
--             show vclock
--           atomically (STMMap.insert vclock key cache)

--     , riakCacheDelete =
--         \key -> do
--           debug $ "[riak] cache delete: " ++ show key
--           atomically (STMMap.delete key cache)
--     }
