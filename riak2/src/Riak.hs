-- TODO export functions from Riak

module Riak
  ( -- * Server info
    ping
  , getServerInfo
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
    -- * Types
  , Bucket(..)
  , BucketProperties(..)
  , BucketType(..)
  , Client
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
  , Index(..)
  , Key(..)
  , generatedKey
  , NotfoundBehavior(..)
  , Object(..)
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
    -- ** Re-exports
  , def
  ) where

import Riak.Bucket                (Bucket(..))
import Riak.BucketProperties      (BucketProperties(..), ConflictResolution(..),
                                   NotfoundBehavior(..))
import Riak.BucketType            (BucketType(..))
import Riak.Client
import Riak.Content
import Riak.Context
import Riak.ConvergentCounter
import Riak.ConvergentHyperLogLog
import Riak.ConvergentMap
import Riak.ConvergentSet
import Riak.ExactQuery            (ExactQuery(..))
import Riak.Index                 (Index(..))
import Riak.Key
import Riak.Object
import Riak.Opts                  (GetOpts(..), PutOpts(..))
import Riak.Quorum                (Quorum(..))
import Riak.RangeQuery            (RangeQuery(..))
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo

import Data.Default.Class (def)
