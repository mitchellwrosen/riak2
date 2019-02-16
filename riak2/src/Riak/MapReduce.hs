-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/mapreduce/>
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/app-guide/advanced-mapreduce/>
module Riak.MapReduce
  ( mapReduceBucket
  , mapReduceKeys
    -- TODO map reduce functions for other input types
  , MapReducePhase(..)
  , MapReduceFunction(..)
  ) where

import Libriak.Handle                  (Handle)
import Riak.Internal.Bucket            (Bucket)
import Riak.Internal.ErlangTerm        (ErlangTerm(..))
import Riak.Internal.Key               (Key)
import Riak.Internal.MapReduceFunction (MapReduceFunction(..))
import Riak.Internal.MapReduceInput    (MapReduceInput(..))
import Riak.Internal.MapReducePhase    (MapReducePhase(..))
import Riak.Internal.Prelude

import qualified Libriak.Handle               as Handle
import qualified Libriak.Proto                as Proto
import qualified Riak.Internal.ErlangTerm     as ErlangTerm
import qualified Riak.Internal.MapReduceInput as MapReduceInput
import qualified Riak.Internal.MapReducePhase as MapReducePhase

import Control.Foldl (FoldM)
import Control.Lens  ((.~))

import qualified Data.Vector as Vector


-- | Perform a MapReduce job over all keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use
-- 'mapReduceQueryExact' with the 'Riak.ExactQuery.inBucket' query.
mapReduceBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either Handle.Error r)
mapReduceBucket handle bucket phases responseFold =
  liftIO (mapReduce_ handle (MapReduceInputBucket bucket) phases responseFold)

-- | Perform a MapReduce job over a list of keys.
mapReduceKeys ::
     MonadIO m
  => Handle -- ^
  -> [Key] -- ^
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either Handle.Error r)
mapReduceKeys handle keys phases responseFold =
  liftIO (mapReduce_ handle (MapReduceInputKeys keys) phases responseFold)

mapReduce_ ::
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either Handle.Error r)
mapReduce_ handle input phases responseFold =
  Handle.mapReduce handle request responseFold

  where
    request :: Proto.RpbMapRedReq
    request =
      Proto.defMessage
        & Proto.contentType .~ "application/x-erlang-binary"
        & Proto.request .~ ErlangTerm.build (makeMapReduceErlangTerm input phases)

-- [{inputs, Inputs}, {query, Query}, {timeout, Timeout}]
--
-- timeout is optional
makeMapReduceErlangTerm ::
     MapReduceInput
  -> [MapReducePhase]
  -> ErlangTerm
makeMapReduceErlangTerm input phases =
  ErlangTerm.list (Vector.fromList [inputTerm, phasesTerm])

  where
    inputTerm :: ErlangTerm
    inputTerm =
      ErlangTerm.tuple2
        (ErlAtomUtf8 "inputs")
        (MapReduceInput.toErlangTerm input)

    phasesTerm :: ErlangTerm
    phasesTerm =
      ErlangTerm.tuple2
        (ErlAtomUtf8 "query")
        (ErlangTerm.list
          (Vector.fromList (map MapReducePhase.toErlangTerm phases)))
