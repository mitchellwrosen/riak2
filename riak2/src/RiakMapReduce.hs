-- TODO map reduce functions for other input types

module RiakMapReduce
  ( mapReduceBucket
  , mapReduceKeys
  ) where

import Libriak.Handle (Handle)
import RiakBucket     (Bucket)

import RiakErlangTerm     (ErlangTerm(..))
import RiakError
import RiakKey            (Key)
import RiakMapReduceInput (MapReduceInput(..))
import RiakMapReducePhase (MapReducePhase(..))
import RiakUtils          (retrying)

import qualified Libriak.Handle     as Handle
import qualified Libriak.Proto      as Proto
import qualified RiakErlangTerm     as ErlangTerm
import qualified RiakMapReduceInput as MapReduceInput
import qualified RiakMapReducePhase as MapReducePhase

import Control.Foldl      (FoldM)
import Control.Lens       ((.~))
import Data.Text.Encoding (decodeUtf8)

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
  -> m (Either MapReduceBucketError r)
mapReduceBucket handle bucket phases responseFold = liftIO $
  doMapReduce handle (MapReduceInputBucket bucket) phases responseFold >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseMapReduceBucketError err))

    Right (Right result) ->
      pure (Right result)

parseMapReduceBucketError :: ByteString -> MapReduceBucketError
parseMapReduceBucketError err =
  UnknownError (decodeUtf8 err)

-- | Perform a MapReduce job over a list of keys.
mapReduceKeys ::
     MonadIO m
  => Handle -- ^
  -> [Key] -- ^
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either Handle.HandleConnectionError (Either ByteString r))
mapReduceKeys handle keys phases responseFold =
  liftIO (doMapReduce handle (MapReduceInputKeys keys) phases responseFold)

doMapReduce ::
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either Handle.HandleConnectionError (Either ByteString r))
doMapReduce handle input phases responseFold =
  retrying 1000000 (doMapReduce_ handle input phases responseFold)

doMapReduce_ ::
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Maybe (Either Handle.HandleConnectionError (Either ByteString r)))
doMapReduce_ handle input phases responseFold =
  Handle.mapReduce handle request responseFold >>= \case
    Left err ->
      pure (Just (Left err))

    Right (Left err)
      | isUnknownMessageCode err ->
          pure Nothing
      | otherwise ->
          pure (Just (Right (Left err)))

    Right (Right result) ->
      pure (Just (Right (Right result)))

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
