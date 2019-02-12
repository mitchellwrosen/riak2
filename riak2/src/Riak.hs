module Riak
  ( -- * Object operations
    -- ** Get object
    get
  , getHead
  , getIfModified
  , getHeadIfModified
    -- ** Put object
  , put
  , putGet
  , putGetHead
    -- ** Delete object
  , delete
    -- * Counter operations
  , getConvergentCounter
  , updateConvergentCounter
    -- * HyperLogLog operations
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
    -- * Map operations
  , getConvergentMap
  , updateConvergentMap
    -- * Set operations
  , getConvergentSet
  , updateConvergentSet
    -- * Bucket type operations
    -- ** Bucket type properties
  , getBucketType
  , setBucketType
    -- ** Full bucket traversals
  , listBuckets
  , streamBuckets
    -- * Bucket operations
    -- ** Bucket properties
  , getBucket
  , setBucket
  , resetBucket
    -- ** Secondary index search
  , queryExact
  , queryRange
    -- ** Full key traversals
  , listKeys
  , streamKeys
    -- * MapReduce operations
  , mapReduceBucket
  , mapReduceKeys
    -- * Search operations
  , search
    -- ** Search schema
  , getSchema
  , putSchema
    -- ** Search index
  , getIndex
  , getIndexes
  , putIndex
  , deleteIndex
    -- * Server info
  , ping
  , getServerInfo
    -- * Types
  , Bucket(..)
  , BucketProperties(..)
  , BucketType(..)
  , ConflictResolution(..)
  , Content(..)
  , newContent
  , Context
  , newContext
  , unsafeMakeContext
  , ConvergentCounter(..)
  , ConvergentHyperLogLog(..)
  , ConvergentMap(..)
  , ConvergentMapUpdate(..)
  , ConvergentMapValue(..)
  , ConvergentSet(..)
  , ConvergentSetUpdate(..)
  , ErlangTerm(..)
  , ExactQuery(..)
  , DeleteOpts(..)
  , GetOpts(..)
  , Handle
  , Index(..)
  , Key(..)
  , generatedKey
  , MapReduceFunction(..)
  , MapReducePhase(..)
  , NotfoundBehavior(..)
  , Object(..)
  , newObject
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , Schema(..)
  , SearchOpts(..)
  , SearchResults(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , Sibling(..)
  , UnexpectedResponse(..)
    -- ** Re-exports
  , def
  ) where

-- TODO rename Config/Error export them
import Libriak.Handle             (Handle, UnexpectedResponse(..))
import Riak.Bucket
import Riak.BucketProperties      (BucketProperties(..), ConflictResolution(..),
                                   NotfoundBehavior(..))
import Riak.BucketType
import Riak.Content
import Riak.Context
import Riak.ConvergentCounter
import Riak.ConvergentHyperLogLog
import Riak.ConvergentMap
import Riak.ConvergentSet
import Riak.ErlangTerm
import Riak.ExactQuery            (ExactQuery(..))
import Riak.Index
import Riak.Key
import Riak.MapReduce
import Riak.Object
import Riak.Quorum                (Quorum(..))
import Riak.RangeQuery            (RangeQuery(..))
import Riak.Schema
import Riak.Search
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo
import Riak.Sibling

import qualified Libriak.Handle as Handle

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class     (def)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.Error ())
ping handle =
  liftIO (Handle.ping handle)
