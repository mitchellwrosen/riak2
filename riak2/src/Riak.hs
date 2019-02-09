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
  , PutOpts(..)
  , Quorum(..)
  , RangeQuery(..)
  , SecondaryIndex(..)
  , SecondaryIndexValue(..)
  , ServerInfo(..)
  , UnexpectedResponse(..)
    -- ** Re-exports
  , def
  ) where

import Riak.Bucket                (Bucket(..))
import Riak.BucketProperties      (BucketProperties(..), ConflictResolution(..),
                                   NotfoundBehavior(..))
import Riak.BucketType            (BucketType(..))
import Riak.Content
import Riak.Context
import Riak.ConvergentCounter
import Riak.ConvergentHyperLogLog
import Riak.ConvergentMap
import Riak.ConvergentSet
import Riak.ExactQuery            (ExactQuery(..))
import Riak.Handle                (Handle, UnexpectedResponse(..))
import Riak.Index                 (Index(..))
import Riak.Key
import Riak.Object
import Riak.Opts                  (GetOpts(..), PutOpts(..))
import Riak.Quorum                (Quorum(..))
import Riak.RangeQuery            (RangeQuery(..))
import Riak.SecondaryIndex        (SecondaryIndex(..))
import Riak.SecondaryIndexValue   (SecondaryIndexValue(..))
import Riak.ServerInfo

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
