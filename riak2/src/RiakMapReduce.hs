-- TODO map reduce functions for other input types

module RiakMapReduce
  ( mapReduceBucket
  , mapReduceKeys
  ) where

import Libriak.Connection (ConnectionError)
import Libriak.Handle     (Handle)
import RiakBucket         (Bucket)

import RiakErlangTerm     (ErlangTerm(..))
import RiakKey            (Key)
import RiakMapReduceInput (MapReduceInput(..))
import RiakMapReducePhase (MapReducePhase(..))

import qualified Libriak.Handle     as Handle
import qualified Libriak.Proto      as Proto
import qualified RiakErlangTerm     as ErlangTerm
import qualified RiakMapReduceInput as MapReduceInput
import qualified RiakMapReducePhase as MapReducePhase

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
  -> m (Either ConnectionError (Either ByteString r))
mapReduceBucket handle bucket phases responseFold =
  liftIO (mapReduce_ handle (MapReduceInputBucket bucket) phases responseFold)

-- | Perform a MapReduce job over a list of keys.
mapReduceKeys ::
     MonadIO m
  => Handle -- ^
  -> [Key] -- ^
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either ConnectionError (Either ByteString r))
mapReduceKeys handle keys phases responseFold =
  liftIO (mapReduce_ handle (MapReduceInputKeys keys) phases responseFold)

mapReduce_ ::
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either ConnectionError (Either ByteString r))
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
