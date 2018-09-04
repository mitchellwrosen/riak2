{-# LANGUAGE NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}

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
-- TODO expose cache interface in .Internal
-- TODO don't accidentally cache data types' vclocks

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
        \loc -> do
          vclock <- atomically (STMMap.lookup (SomeRiakKey loc) cache)
          -- debug $
          --   "[riak] cache lookup: " ++ show vclock ++ " <= " ++
          --   show loc
          pure vclock

    , riakCacheInsert =
        \loc vclock -> do
          -- debug $
          --   "[riak] cache insert: " ++ show loc ++ " => " ++
          --   show vclock
          atomically (STMMap.insert vclock (SomeRiakKey loc) cache)

    , riakCacheDelete =
        \loc -> do
          -- debug $ "[riak] cache delete: " ++ show loc
          atomically (STMMap.delete (SomeRiakKey loc) cache)
    }
