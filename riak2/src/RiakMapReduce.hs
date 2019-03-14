-- TODO add modfun mapreduce
-- TODO better map reduce fold types

module RiakMapReduce
  ( mapReduceKeys
  , mapReduceBucket
  , mapReduceBinaryIndex
  , mapReduceIntIndex
  , mapReduceSearch
  ) where

import RiakBucket      (Bucket)
import RiakHandle      (Handle)
import RiakHandleError (HandleError)

import RiakBinaryIndexQuery (BinaryIndexQuery)
import RiakErlangTerm       (ErlangTerm(..))
import RiakError
import RiakIndexName        (IndexName)
import RiakIntIndexQuery    (IntIndexQuery)
import RiakKey              (Key)
import RiakMapReduceInput   (MapReduceInput(..))
import RiakMapReducePhase   (MapReducePhase(..))
import RiakUtils            (retrying)

import qualified RiakErlangTerm     as ErlangTerm
import qualified RiakHandle         as Handle
import qualified RiakMapReduceInput as MapReduceInput
import qualified RiakMapReducePhase as MapReducePhase

import Control.Foldl      (FoldM)
import Control.Lens       ((.~))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto
import qualified Data.Vector     as Vector


-- | Perform a MapReduce job over a list of keys.
--
-- TODO test mapReduceKeys
-- TODO MapReduceKeysError
mapReduceKeys ::
     MonadIO m
  => Handle -- ^
  -> [Key] -- ^
  -> [MapReducePhase] -- ^
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either HandleError (Either ByteString r))
mapReduceKeys handle keys phases responseFold = liftIO $
  doMapReduce handle (MapReduceInputKeys keys) phases responseFold

-- | Perform a MapReduce job over all keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use
-- 'mapReduceBinaryIndex' with the 'Riak.SecondaryIndexQuery.inBucket' query.
--
-- TODO test mapReduceBucket
mapReduceBucket ::
     forall m r.
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> [MapReducePhase] -- ^
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either MapReduceBucketError r)
mapReduceBucket handle bucket phases responseFold = liftIO $
  fromResponse <$>
    doMapReduce handle (MapReduceInputBucket bucket) phases responseFold

  where
    fromResponse ::
         Either HandleError (Either ByteString r)
      -> Either MapReduceBucketError r
    fromResponse = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseMapReduceBucketError err)

      Right (Right result) ->
        Right result

parseMapReduceBucketError :: ByteString -> MapReduceBucketError
parseMapReduceBucketError err =
  UnknownError (decodeUtf8 err)

-- | Perform a MapReduce job over the results of a binary secondary index query.
--
-- TODO test mapReduceBinaryIndex
mapReduceBinaryIndex ::
     forall m r.
     MonadIO m
  => Handle -- ^
  -> BinaryIndexQuery -- ^
  -> [MapReducePhase] -- ^
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either MapReduceBinaryIndexError r)
mapReduceBinaryIndex handle query phases responseFold = liftIO $
  fromResponse <$>
    doMapReduce handle (MapReduceInputBinaryIndexQuery query) phases responseFold

  where
    fromResponse ::
         Either HandleError (Either ByteString r)
      -> Either MapReduceBinaryIndexError r
    fromResponse = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseMapReduceBinaryIndexError err)

      Right (Right result) ->
        Right result

parseMapReduceBinaryIndexError :: ByteString -> MapReduceBinaryIndexError
parseMapReduceBinaryIndexError err =
  UnknownError (decodeUtf8 err)

-- | Perform a MapReduce job over the results of a integer secondary index
-- query.
--
-- TODO test mapReduceIntIndex
mapReduceIntIndex ::
     forall m r.
     MonadIO m
  => Handle -- ^
  -> IntIndexQuery -- ^
  -> [MapReducePhase] -- ^
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either MapReduceIntIndexError r)
mapReduceIntIndex handle query phases responseFold = liftIO $
  fromResponse <$>
    doMapReduce handle (MapReduceInputIntIndexQuery query) phases responseFold

  where
    fromResponse ::
         Either HandleError (Either ByteString r)
      -> Either MapReduceIntIndexError r
    fromResponse = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseMapReduceIntIndexError err)

      Right (Right result) ->
        Right result

parseMapReduceIntIndexError :: ByteString -> MapReduceIntIndexError
parseMapReduceIntIndexError err =
  UnknownError (decodeUtf8 err)

-- | Perform a MapReduce job over the results of a search query.
--
-- -- TODO test mapReduceSearch
-- -- TODO MapReduceSearchError
-- -- TODO oops, for mapReduceSearch UnknownMessageCode might refer to yokozuna
mapReduceSearch ::
     MonadIO m
  => Handle -- ^
  -> IndexName -- ^
  -> ByteString -- ^ Search query
  -> [MapReducePhase] -- ^
  -> FoldM IO Proto.RpbMapRedResp r -- ^
  -> m (Either HandleError (Either ByteString r))
mapReduceSearch handle index query phases responseFold = liftIO $
  doMapReduce handle (MapReduceInputSearch index query) phases responseFold

doMapReduce ::
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either HandleError (Either ByteString r))
doMapReduce handle input phases responseFold =
  retrying 1000000 (doMapReduce_ handle input phases responseFold)

doMapReduce_ ::
     forall r.
     Handle
  -> MapReduceInput
  -> [MapReducePhase]
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Maybe (Either HandleError (Either ByteString r)))
doMapReduce_ handle input phases responseFold =
  fromResponse <$>
    Handle.mapReduce
      handle
      request
      responseFold

  where
    request :: Proto.RpbMapRedReq
    request =
      Proto.defMessage
        & Proto.contentType .~ "application/x-erlang-binary"
        & Proto.request .~ ErlangTerm.build (makeMapReduceErlangTerm input phases)

    fromResponse ::
         Either HandleError (Either ByteString r)
      -> Maybe (Either HandleError (Either ByteString r))
    fromResponse = \case
      Left err ->
        Just (Left err)

      Right (Left err)
        | isUnknownMessageCode err ->
            Nothing
        | otherwise ->
            Just (Right (Left err))

      Right (Right result) ->
        Just (Right (Right result))

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
