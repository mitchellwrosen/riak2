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
  , keyBucket
  , generatedKey
  , ListBucketsError
  , ListKeysError
  , MapReduceBucketError
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
  , QueryExactError
  , QueryRangeError
  , Quorum(..)
  , RangeQuery(..)
  , ReadQuorum(..)
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
  , WriteQuorum(..)
    -- ** Re-exports
  , def
  ) where

-- TODO rename Config/Error export them
import Libriak.Handle             (Handle, UnexpectedResponse(..))
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
import Riak.Error
import Riak.ExactQuery            (ExactQuery(..))
import Riak.Index
import Riak.Key
import Riak.MapReduce
import Riak.Object
import Riak.Quorum                (Quorum(..), ReadQuorum(..), WriteQuorum(..))
import Riak.RangeQuery            (RangeQuery(..))
import Riak.Schema
import Riak.Search
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo
import Riak.Sibling
import RiakBucket
import RiakPing

import Data.Default.Class (def)
