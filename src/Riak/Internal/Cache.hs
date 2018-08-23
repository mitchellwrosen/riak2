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
  = Cache (STM.Map SomeLocation Vclock)


newCache :: IO Cache
newCache =
  Cache <$> STMMap.newIO

cacheLookup :: Cache -> Location ty -> IO (Maybe Vclock)
cacheLookup (Cache cache) loc =
  atomically (STMMap.lookup (SomeLocation loc) cache)

cacheInsert :: Cache -> Location ty -> Vclock -> IO ()
cacheInsert (Cache cache) loc vclock =
  atomically (STMMap.insert vclock (SomeLocation loc) cache)

cacheDelete :: Cache -> Location ty -> IO ()
cacheDelete (Cache cache) loc =
  atomically (STMMap.delete (SomeLocation loc) cache)
