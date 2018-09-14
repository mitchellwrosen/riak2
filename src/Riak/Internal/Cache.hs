module Riak.Internal.Cache
  ( RiakCache(..)
  , newSTMRiakCache
  ) where

import Control.Concurrent.STM

import qualified StmContainers.Map as STM (Map)
import qualified StmContainers.Map as STMMap

import Riak.Internal.Debug
import Riak.Internal.Prelude
import Riak.Internal.Types

-- TODO cache ttl

data RiakCache
  = RiakCache
  { riakCacheLookup :: forall ty. RiakKey ty -> IO (Maybe RiakVclock)
  , riakCacheInsert :: forall ty. RiakKey ty -> RiakVclock -> IO ()
  , riakCacheDelete :: forall ty. RiakKey ty -> IO ()
  }

-- | Create a 'RiakCache' backed by an STM map.
newSTMRiakCache :: IO RiakCache
newSTMRiakCache = do
  cache :: STM.Map SomeRiakKey RiakVclock <-
    STMMap.newIO

  pure RiakCache
    { riakCacheLookup = do
        \key -> do
          vclock <- atomically (STMMap.lookup (SomeRiakKey key) cache)
          debug $
            "[riak] cache lookup: " ++ show vclock ++ " <= " ++
            show key
          pure vclock

    , riakCacheInsert =
        \key vclock -> do
          debug $
            "[riak] cache insert: " ++ show key ++ " => " ++
            show vclock
          atomically (STMMap.insert vclock (SomeRiakKey key) cache)

    , riakCacheDelete =
        \key -> do
          debug $ "[riak] cache delete: " ++ show key
          atomically (STMMap.delete (SomeRiakKey key) cache)
    }
