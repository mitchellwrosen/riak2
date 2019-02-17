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
    -- * Convergent counter operations
  , getConvergentCounter
  , updateConvergentCounter
    -- * Convergent HyperLogLog operations
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
    -- * Convergent map operations
  , getConvergentMap
  , putConvergentMap
    -- * Convergent set operations
  , getConvergentSet
  , putConvergentSet
    -- * Bucket type operations
    -- ** Bucket type properties
  , getBucketType
  , setBucketTypeIndex
  , unsetBucketTypeIndex
    -- ** Full bucket traversals
  , listBuckets
  , streamBuckets
    -- * Bucket operations
    -- ** Bucket properties
  , getBucket
  , setBucketIndex
  , unsetBucketIndex
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
  , BucketType
  , defaultBucketType
  , ConflictResolution(..)
  , Content(..)
  , newContent
  , Context
  , newContext
  , unsafeMakeContext
  , ConvergentCounter(..)
  , ConvergentHyperLogLog(..)
  , ConvergentMap
  , newConvergentMap
  , convergentMapKey
  , convergentMapValue
  , ConvergentMapValue(..)
  , emptyConvergentMapValue
  , ConvergentSet
  , newConvergentSet
  , convergentSetKey
  , convergentSetValue
  , ErlangTerm(..)
  , ExactQuery(..)
  , DeleteOpts(..)
  , GetOpts(..)
  , Handle
  , Index(..)
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  , Key(..)
  , generatedKey
  , MapReduceFunction(..)
  , MapReducePhase(..)
  , NotfoundBehavior(..)
  , Object(..)
  , newObject
  , PutIndexOpts(..)
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , Schema(..)
  , defaultSchema
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
import Data.ByteString        (ByteString)
import Data.Default.Class     (def)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.HandleError (Either ByteString ()))
ping handle =
  liftIO (Handle.ping handle)
