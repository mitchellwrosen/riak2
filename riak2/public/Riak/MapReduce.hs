-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/mapreduce/>
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/app-guide/advanced-mapreduce/>
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/mapreduce/>

module Riak.MapReduce
  ( mapReduceKeys
  , mapReduceBucket
  , mapReduceBinaryIndex
  , mapReduceIntIndex
  , mapReduceSearch
  , MapReducePhase(..)
  , MapReduceResult(..)
  ) where

import RiakMapReduce
import RiakMapReducePhase
import RiakMapReduceResult (MapReduceResult(..))
