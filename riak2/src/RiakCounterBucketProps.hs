module RiakCounterBucketProps where

import RiakIndexName        (IndexName)
import RiakNotfoundBehavior (NotfoundBehavior)
import RiakReadQuorum       (ReadQuorum)
import RiakWriteQuorum      (WriteQuorum)

import qualified Data.Riak.Proto as Proto


data CounterBucketProps
  = CounterBucketProps
  { index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , notfoundBehavior :: NotfoundBehavior
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , readQuorum :: ReadQuorum
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)
