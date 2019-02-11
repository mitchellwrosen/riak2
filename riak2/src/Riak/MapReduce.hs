module Riak.MapReduce
  ( mapReduce
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

import Control.Foldl (FoldM)


mapReduce ::
     MonadIO m
  => Handle -- ^
  -> Proto.MapReduceRequest -- ^
  -> FoldM IO Proto.MapReduceResponse r -- ^
  -> m (Either Handle.Error r)
mapReduce handle request responseFold =
  liftIO (Handle.mapReduce handle request responseFold)
