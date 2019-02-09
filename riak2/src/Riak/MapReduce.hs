module Riak.MapReduce
  ( mapReduce
  ) where

import Riak.Handle           (Handle)
import Riak.Internal.Prelude

import qualified Riak.Handle as Handle
import qualified Riak.Proto  as Proto

import Control.Foldl (FoldM)


mapReduce ::
     MonadIO m
  => Handle
  -> Proto.MapReduceRequest
  -> FoldM IO Proto.MapReduceResponse r
  -> m (Either ByteString r)
mapReduce handle request responseFold =
  liftIO (Handle.mapReduce handle request responseFold)
