{-# LANGUAGE NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}

module Riak.Internal.Cache
  ( Cache
  , newCache
  , cacheLookup
  , cacheInsert
  , cacheDelete
  ) where

import Control.Concurrent.STM

import qualified StmContainers.Map as STM (Map)
import qualified StmContainers.Map as STMMap

import Riak.Internal.Prelude
import Riak.Internal.Types

newtype Cache
  = Cache (STM.Map SomeRiakLocation RiakVclock)


newCache :: IO Cache
newCache =
  Cache <$> STMMap.newIO

cacheLookup :: Cache -> RiakLocation ty -> IO (Maybe RiakVclock)
cacheLookup (Cache cache) loc =
  atomically (STMMap.lookup (SomeRiakLocation loc) cache)

cacheInsert :: Cache -> RiakLocation ty -> RiakVclock -> IO ()
cacheInsert (Cache cache) loc vclock =
  atomically (STMMap.insert vclock (SomeRiakLocation loc) cache)

cacheDelete :: Cache -> RiakLocation ty -> IO ()
cacheDelete (Cache cache) loc =
  atomically (STMMap.delete (SomeRiakLocation loc) cache)
