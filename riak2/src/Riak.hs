-- TODO export functions from Riak

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
    -- * Solr operations
    -- ** Schema
  , getSchema
  , putSchema
    -- ** Index
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
  , ExactQuery(..)
  , GetOpts(..)
  , Handle
  , Index(..)
  , Key(..)
  , generatedKey
  , NotfoundBehavior(..)
  , Object(..)
  , newObject
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , Schema(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , Sibling(..)
  , UnexpectedResponse(..)
    -- ** Re-exports
  , def
  ) where

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
import Riak.ExactQuery            (ExactQuery(..))
import Riak.Handle                (Handle, UnexpectedResponse(..))
import Riak.Index
import Riak.Key
import Riak.Object
import Riak.Opts                  (GetOpts(..), PutOpts(..))
import Riak.Quorum                (Quorum(..))
import Riak.RangeQuery            (RangeQuery(..))
import Riak.Schema
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo
import Riak.Sibling

import qualified Riak.Handle as Handle

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class     (def)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.Error ())
ping handle =
  liftIO (Handle.ping handle)
