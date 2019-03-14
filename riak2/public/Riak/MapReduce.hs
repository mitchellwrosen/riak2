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
  , MapReduceFunction(..)
  ) where

import RiakMapReduce
import RiakMapReduceFunction
import RiakMapReducePhase
