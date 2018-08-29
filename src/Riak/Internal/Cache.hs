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

data RiakCache
  = RiakCache
  { riakCacheLookup :: forall ty. RiakLocation ty -> IO (Maybe RiakVclock)
  , riakCacheInsert :: forall ty. RiakLocation ty -> RiakVclock -> IO ()
  , riakCacheDelete :: forall ty. RiakLocation ty -> IO ()
  }

-- | Create a 'RiakCache' backed by an STM map.
newSTMRiakCache :: IO RiakCache
newSTMRiakCache = do
  cache :: STM.Map SomeRiakLocation RiakVclock <-
    STMMap.newIO

  pure RiakCache
    { riakCacheLookup = do
        \loc -> do
          vclock <- atomically (STMMap.lookup (SomeRiakLocation loc) cache)
          debug $
            "[riak] cache lookup: " ++ show vclock ++ " <= " ++
            debugShowLocation loc
          pure vclock

    , riakCacheInsert =
        \loc vclock -> do
          debug $
            "[riak] cache insert: " ++ debugShowLocation loc ++ " => " ++
            show vclock
          atomically (STMMap.insert vclock (SomeRiakLocation loc) cache)

    , riakCacheDelete =
        \loc -> do
          debug $ "[riak] cache delete: " ++ debugShowLocation loc
          atomically (STMMap.delete (SomeRiakLocation loc) cache)
    }

debugShowLocation :: RiakLocation ty -> String
debugShowLocation (RiakLocation (RiakNamespace type' bucket) key) =
  show type' ++ " " ++ show bucket ++ " " ++ show key
