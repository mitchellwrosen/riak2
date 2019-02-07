module Riak.MapReduce
  ( mapReduce
  ) where

import Riak.Client (Client)
import Riak.Internal.Prelude

import qualified Riak.Interface as Interface
import qualified Riak.Proto as Proto

import Control.Foldl (FoldM)


mapReduce ::
     MonadIO m
  => Client
  -> Proto.MapReduceRequest
  -> FoldM IO Proto.MapReduceResponse r
  -> m (Either ByteString r)
mapReduce client request responseFold =
  liftIO (Interface.mapReduce client request responseFold)
