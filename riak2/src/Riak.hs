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
    -- * Types
  , Bucket(..)
  , BucketProperties(..)
  , BucketType(..)
  , Client
  , ConflictResolution(..)
  , Content(..)
  , Context
  , newContext
  , Counter(..)
  , ExactQuery(..)
  , GetOpts(..)
  , HyperLogLog(..)
  , Index(..)
  , Key(..)
  , generatedKey
  , Map(..)
  , MapUpdate(..)
  , Maps(..)
  , NotfoundBehavior(..)
  , Object(..)
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , Set(..)
  , SetUpdate(..)
    -- ** Re-exports
  , def
  ) where

import Riak.Bucket              (Bucket(..))
import Riak.BucketProperties    (BucketProperties(..), ConflictResolution(..),
                                 NotfoundBehavior(..))
import Riak.BucketType          (BucketType(..))
import Riak.Client
import Riak.Content             (Content(..))
import Riak.Context
import Riak.Counter             (Counter(..))
import Riak.ExactQuery          (ExactQuery(..))
import Riak.HyperLogLog         (HyperLogLog(..))
import Riak.Index               (Index(..))
import Riak.Key
import Riak.Map                 (Map(..), MapUpdate(..), Maps(..))
import Riak.Object
import Riak.Opts                (GetOpts(..), PutOpts(..))
import Riak.Quorum              (Quorum(..))
import Riak.RangeQuery          (RangeQuery(..))
import Riak.SecondaryIndex      (SecondaryIndex(..))
import Riak.SecondaryIndexValue (SecondaryIndexValue(..))
import Riak.ServerInfo
import Riak.Set                 (Set(..), SetUpdate(..))

import Data.Default.Class (def)
