-- |
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <http://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>

module Riak.ConvergentHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
  ) where

import RiakConvergentHyperLogLog
