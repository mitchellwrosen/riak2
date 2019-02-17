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
  , Error(..)
  , ExactQuery(..)
  , DeleteError
  , DeleteIndexError
  , DeleteOpts(..)
  , GetBucketError
  , GetBucketTypeError
  , GetConvergentCounterError
  , GetConvergentHyperLogLogError
  , GetConvergentMapError
  , GetConvergentSetError
  , GetError
  , GetIndexError
  , GetOpts(..)
  , GetSchemaError
  , Handle
  , Index(..)
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  , Key(..)
  , generatedKey
  , ListBucketsError
  , ListKeysError
  , MapReduceFunction(..)
  , MapReducePhase(..)
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  , MayReturnOverload
  , MayReturnSearchNotEnabled
  , NotfoundBehavior(..)
  , Object(..)
  , newObject
  , Op(..)
  , PutConvergentMapError
  , PutConvergentSetError
  , PutError
  , PutIndexError
  , PutSchemaError
  , PutIndexOpts(..)
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , Schema(..)
  , defaultSchema
  , SearchError
  , SearchOpts(..)
  , SearchResults(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , SetBucketTypeIndexError
  , Sibling(..)
  , UnexpectedResponse(..)
  , UpdateConvergentCounterError
  , UpdateConvergentHyperLogLogError
    -- ** Re-exports
  , def
  ) where

-- TODO rename Config/Error export them
import Libriak.Handle            (Handle, UnexpectedResponse(..))
import RiakBucket
import RiakBucketProperties      (BucketProperties(..), ConflictResolution(..),
                                  NotfoundBehavior(..))
import RiakBucketType
import RiakContent
import RiakContext
import RiakConvergentCounter
import RiakConvergentHyperLogLog
import RiakConvergentMap
import RiakConvergentSet
import RiakErlangTerm
import RiakError
import RiakExactQuery            (ExactQuery(..))
import RiakIndex
import RiakKey
import RiakMapReduce
import RiakObject
import RiakQuorum                (Quorum(..))
import RiakRangeQuery            (RangeQuery(..))
import RiakSchema
import RiakSearch
import RiakSecondaryIndex        (SecondaryIndex(..))
import RiakSecondaryIndexValue   (SecondaryIndexValue(..))
import RiakServerInfo
import RiakSibling

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
