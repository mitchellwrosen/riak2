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
  , getCounter
  , updateCounter
    -- * Convergent HyperLogLog operations
  , getHyperLogLog
  , updateHyperLogLog
    -- * Convergent map operations
  , getMap
  , putMap
    -- * Convergent set operations
  , getSet
  , putSet
    -- * Bucket type operations
    -- ** Bucket type properties
  , getBucketType
  , getCounterBucketType
  , getHyperLogLogBucketType
  , getMapBucketType
  , getSetBucketType
  , setBucketTypeIndex
  , unsetBucketTypeIndex
    -- ** Full bucket traversals
  , listBuckets
  , streamBuckets
    -- * Bucket operations
    -- ** Bucket properties
  , getBucket
  , getCounterBucket
  , getHyperLogLogBucket
  , getMapBucket
  , getSetBucket
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
  , mapReduceKeys
  , mapReduceBucket
  , mapReduceBinaryIndex
  , mapReduceIntIndex
  , mapReduceSearch
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
  , keysBetween
  , Bucket(..)
  , bucketBucketType
  , bucketBucketSegment
  , BucketProps(..)
  , BucketType
  , defaultBucketType
  , ConflictResolution(..)
  , Content(..)
  , newContent
  , Context
  , emptyContext
  , unsafeMakeContext
  , ConvergentCounter(..)
  , ConvergentHyperLogLog(..)
  , ConvergentMap
  , newMap
  , mapKey
  , mapValue
  , ConvergentMapValue(..)
  , emptyMapValue
  , ConvergentSet
  , newSet
  , setKey
  , setValue
  , CounterBucketProps(..)
  , ErlangTerm(..)
  , Error(..)
  , EventHandlers(..)
  , DeleteIndexError
  , GetBucketError
  , GetBucketTypeError
  , GetCounterError
  , GetError
  , GetHyperLogLogError
  , GetIndexError
  , GetMapError
  , GetOpts(..)
  , GetSchemaError
  , GetSetError
  , Handle
  , HandleConfig(..)
  , HandleError(..)
  , HyperLogLogBucketProps(..)
  , Index(..)
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  , unIndexName
  , IntIndexQuery(..)
  , Key(..)
  , keyBucketType
  , keyBucket
  , keyBucketSegment
  , keyKeySegment
  , generatedKey
  , ListBucketsError
  , ListKeysError
  , MapBucketProps(..)
  , MapReduceBucketError
  , MapReduceFunction(..)
  , MapReducePhase(..)
  , MayReturnBucketTypeDoesNotExist
  , MayReturnIndexDoesNotExist
  , MayReturnInvalidNodes
  , NotfoundOk(..)
  , Object(..)
  , newObject
  , Op(..)
  , PruneContextSettings(..)
  , PutError
  , PutIndexError
  , PutIndexOpts(..)
  , PutMapError
  , PutOpts(..)
  , PutSchemaError
  , PutSetError
  , QueryIndexError
  , Quorum(..)
  , ReadQuorum(..)
  , Schema(..)
  , defaultSchema
  , SearchError
  , SearchOpts(..)
  , SearchResults(..)
  , SecondaryIndex(..)
  , ServerInfo(..)
  , SetBucketProps(..)
  , SetBucketTypeIndexError
  , Sibling(..)
  , SomeBucketProps(..)
  , UpdateCounterError
  , UpdateHyperLogLogError
  , WriteQuorum(..)
#ifdef DEBUG
  , debug
#endif
  ) where

import Riak.BinaryIndexQuery      (BinaryIndexQuery(..), keysBetween, inBucket)
import Riak.Bucket
import Riak.BucketProps           (BucketProps(..), ConflictResolution(..),
                                   CounterBucketProps(..),
                                   HyperLogLogBucketProps(..),
                                   MapBucketProps(..), PruneContextSettings(..),
                                   SetBucketProps(..), SomeBucketProps(..))
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
import Riak.IntIndexQuery         (IntIndexQuery(..))
import Riak.Key
import Riak.MapReduce
import Riak.Object
import Riak.Quorum                (NotfoundOk(..), Quorum(..), ReadQuorum(..),
                                   WriteQuorum(..))
import Riak.Schema
import Riak.Search
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.ServerInfo
import Riak.Sibling
import RiakPing
#ifdef DEBUG
import RiakDebug (debug)
#endif
