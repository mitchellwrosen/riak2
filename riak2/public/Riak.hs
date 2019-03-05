{-# LANGUAGE CPP #-}

module Riak
  ( -- * Handle
    createHandle
    -- * Object operations
    -- ** Get object
  , get
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
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
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
  , BinaryIndexQuery(..)
  , inBucket
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
  , CounterBucketProperties(..)
  , ErlangTerm(..)
  , Error(..)
  , EventHandlers(..)
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
  , HandleConfig(..)
  , HandleError(..)
  , HyperLogLogBucketProperties(..)
  , Index(..)
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  , IntIndexQuery(..)
  , Key(..)
  , keyBucket
  , generatedKey
  , ListBucketsError
  , ListKeysError
  , MapBucketProperties(..)
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
  , ObjectBucketProperties(..)
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
  , ReadQuorum(..)
  , Schema(..)
  , defaultSchema
  , SearchError
  , SearchOpts(..)
  , SearchResults(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , SetBucketProperties(..)
  , SetBucketTypeIndexError
  , Sibling(..)
  , UpdateConvergentCounterError
  , UpdateConvergentHyperLogLogError
  , WriteQuorum(..)
    -- ** Re-exports
  , Endpoint(..)
  , def
#ifdef DEBUG
  , debug
#endif
  ) where

-- TODO rename Config/Error export them
import Libriak.Connection         (Endpoint(..))
import Riak.BucketProperties      (BucketProperties(..), ConflictResolution(..),
                                   CounterBucketProperties(..),
                                   HyperLogLogBucketProperties(..),
                                   MapBucketProperties(..),
                                   NotfoundBehavior(..),
                                   ObjectBucketProperties(..),
                                   SetBucketProperties(..))
import Riak.BucketType
import Riak.Content
import Riak.Context
import Riak.ConvergentCounter
import Riak.ConvergentHyperLogLog
import Riak.ConvergentMap
import Riak.ConvergentSet
import Riak.ErlangTerm
import Riak.Error
import Riak.Handle                (EventHandlers(..), Handle, HandleConfig(..),
                                   HandleError, createHandle)
import Riak.Index
import Riak.Key
import Riak.MapReduce
import Riak.Object
import Riak.Quorum                (Quorum(..), ReadQuorum(..), WriteQuorum(..))
import Riak.Schema
import Riak.Search
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexQuery   (BinaryIndexQuery(..), IntIndexQuery(..),
                                   inBucket)
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo
import Riak.Sibling
import RiakBucket
import RiakPing
#ifdef DEBUG
import RiakDebug (debug)
#endif

import Data.Default.Class (def)
