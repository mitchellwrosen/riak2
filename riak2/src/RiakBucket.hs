module RiakBucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , getBucket
  , setBucketIndex
  , unsetBucketIndex
  , resetBucket
    -- ** Search
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
    -- ** Full traversals
  , listKeys
  , streamKeys
  ) where

import Libriak.Response     (Response(..))
import RiakBinaryIndexQuery (BinaryIndexQuery(..))
import RiakBucketInternal   (Bucket(..))
import RiakBucketProperties (BucketProperties)
import RiakError
import RiakHandle           (Handle, HandleError)
import RiakIndexName        (IndexName(..))
import RiakIntIndexQuery    (IntIndexQuery(..))
import RiakKey              (Key(..))
import RiakUtils            (bs2int, int2bs, retrying)

import qualified RiakBinaryIndexQuery as BinaryIndexQuery
import qualified RiakBucketProperties as BucketProperties
import qualified RiakHandle           as Handle

import Control.Foldl                      (FoldM(..))
import Control.Lens                       (folded, to, (.~), (^.))
import Data.Profunctor                    (lmap)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Data.Text.Encoding                 (decodeUtf8, encodeUtf8)

import qualified Control.Foldl   as Foldl
import qualified Data.Riak.Proto as Proto


-- | Get bucket properties.
getBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetBucketError (Maybe BucketProperties))
getBucket handle bucket = liftIO $
  Handle.getBucket handle request >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (parseGetBucketError err)

    Right (Right (RespRpbGetBucket response)) ->
      pure (Right (Just (BucketProperties.fromProto (response ^. Proto.props))))

  where
    request :: Proto.RpbGetBucketReq
    request =
      Proto.defMessage
        & setProto bucket

parseGetBucketError ::
     ByteString
  -> Either GetBucketError (Maybe BucketProperties)
parseGetBucketError err
  | isBucketTypeDoesNotExistError4 err =
      Right Nothing
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Set the index of a bucket.
setBucketIndex ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> IndexName -- ^ Index name
  -> m (Either SetBucketIndexError ())
setBucketIndex handle bucket@(Bucket bucketType _) index = liftIO $
  Handle.setBucket handle request >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseSetBucketIndexError bucketType index err))

    Right (Right _) ->
      pure (Right ())

  where
    request :: Proto.RpbSetBucketReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 (unIndexName index))

parseSetBucketIndexError ::
     ByteString
  -> IndexName
  -> ByteString
  -> SetBucketIndexError
parseSetBucketIndexError bucketType index err
  | isBucketTypeDoesNotExistError4 err =
      BucketTypeDoesNotExistError bucketType
  | isIndexDoesNotExistError0 err =
      IndexDoesNotExistError index
  | isInvalidNodesError1 err =
      InvalidNodesError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | Unset the index of a bucket.
unsetBucketIndex ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either SetBucketIndexError ())
unsetBucketIndex handle bucket =
  setBucketIndex handle bucket (IndexName "_dont_index_")

-- | Reset bucket properties.
resetBucket ::
     MonadIO m
  => Handle
  -> Bucket
  -> m (Either HandleError (Either ByteString ()))
resetBucket handle (Bucket bucketType bucket) = liftIO $
  (fmap.fmap) (() <$)
    (Handle.resetBucket handle request)

  where
    request :: Proto.RpbResetBucketReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.type' .~ bucketType

-- | Perform a query on a binary secondary index.
--
-- Fetches results in batches of 50.
queryBinaryIndex ::
     MonadIO m
  => Handle -- ^
  -> BinaryIndexQuery -- ^
  -> FoldM IO Key r -- ^
  -> m (Either QueryRangeError r)
queryBinaryIndex handle query@(BinaryIndexQuery { bucket, minValue, maxValue }) keyFold = liftIO $
  doIndex handle request (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.index .~ BinaryIndexQuery.indexName query
        & Proto.maxResults .~ 50 -- TODO configure page size
        & Proto.rangeMax .~ maxValue
        & Proto.rangeMin .~ minValue
        & Proto.qtype .~ Proto.RpbIndexReq'range -- TODO perform exact query if min == max
        & Proto.stream .~ True

    handler :: Foldl.HandlerM IO (Response 26) Key
    handler =
      to (\(RespRpbIndex response) -> response) . Proto.keys . folded . to mkKey

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

-- | Perform a query on a binary secondary index.
--
-- Fetches results in batches of 50.
--
-- TODO make this work for '$bucket' queries
queryBinaryIndexTerms ::
     MonadIO m
  => Handle -- ^
  -> BinaryIndexQuery -- ^
  -> FoldM IO (ByteString, Key) r -- ^
  -> m (Either QueryRangeError r)
queryBinaryIndexTerms handle query@(BinaryIndexQuery { bucket, minValue, maxValue }) keyFold = liftIO $
  doIndex handle request (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.index .~ BinaryIndexQuery.indexName query
        & Proto.maxResults .~ 50 -- TODO configure page size
        & Proto.rangeMax .~ maxValue
        & Proto.rangeMin .~ minValue
        & Proto.returnTerms .~ True
        & Proto.qtype .~ Proto.RpbIndexReq'range
        & Proto.stream .~ True

    handler :: Foldl.HandlerM IO (Response 26) (ByteString, Key)
    handler =
     to (\(RespRpbIndex response) -> response) . Proto.results . folded . to fromResult

    fromResult :: Proto.RpbPair -> (ByteString, Key)
    fromResult pair =
      (pair ^. Proto.key, mkKey (pair ^. Proto.value))

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

-- | Perform a query on an integer secondary index.
--
-- Fetches results in batches of 50.
queryIntIndex ::
     MonadIO m
  => Handle -- ^
  -> IntIndexQuery -- ^
  -> FoldM IO Key r -- ^
  -> m (Either QueryRangeError r)
queryIntIndex handle IntIndexQuery { bucket, index, minValue, maxValue } keyFold = liftIO $
  doIndex handle request (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.index .~ (index <> "_int")
        & Proto.maxResults .~ 50 -- TODO configure page size
        & Proto.rangeMax .~ int2bs maxValue
        & Proto.rangeMin .~ int2bs minValue
        & Proto.qtype .~ Proto.RpbIndexReq'range
        & Proto.stream .~ True

    handler :: Foldl.HandlerM IO (Response 26) Key
    handler =
      to (\(RespRpbIndex response) -> response) . Proto.keys . folded . to mkKey

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

-- | Perform a query on an integer secondary index.
--
-- Fetches results in batches of 50.
queryIntIndexTerms ::
     MonadIO m
  => Handle -- ^
  -> IntIndexQuery -- ^
  -> FoldM IO (Int64, Key) r -- ^
  -> m (Either QueryRangeError r)
queryIntIndexTerms handle IntIndexQuery { bucket, index, minValue, maxValue } keyFold = liftIO $
  doIndex handle request (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.index .~ (index <> "_int")
        & Proto.maxResults .~ 50 -- TODO configure page size
        & Proto.rangeMax .~ int2bs maxValue
        & Proto.rangeMin .~ int2bs minValue
        & Proto.returnTerms .~ True
        & Proto.qtype .~ Proto.RpbIndexReq'range
        & Proto.stream .~ True

    handler :: Foldl.HandlerM IO (Response 26) (Int64, Key)
    handler =
      to (\(RespRpbIndex response) -> response) . Proto.results . folded . to fromResult

    fromResult :: Proto.RpbPair -> (Int64, Key)
    fromResult pair =
      (bs2int (pair ^. Proto.key), mkKey (pair ^. Proto.value))

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

doIndex ::
     forall r.
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO (Response 26) r
  -> IO (Either (Error 'SecondaryIndexQueryOp) r)
doIndex handle request responseFold =
  retrying 1000000 (doIndex_ handle request responseFold)

doIndex_ ::
     forall r.
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO (Response 26) r
  -> IO (Maybe (Either (Error 'SecondaryIndexQueryOp) r))
doIndex_ handle =
  loop

  where
    loop ::
         Proto.RpbIndexReq
      -> FoldM IO (Response 26) r
      -> IO (Maybe (Either (Error 'SecondaryIndexQueryOp) r))
    loop request responseFold = do
      result :: Either HandleError (Either ByteString (FoldM IO (Response 26) r, Maybe ByteString)) <-
        doIndexPage
          handle
          request
          (Foldl.duplicateM responseFold)

      case result of
        Left err ->
          pure (Just (Left (HandleError err)))

        Right (Left err) ->
          pure (Left <$> parseSecondaryIndexQueryError bucket err)

        Right (Right (nextResponseFold, continuation)) ->
          case continuation of
            Nothing ->
              Just . Right <$> extractM nextResponseFold

            Just continuation -> do
              loop
                (request & Proto.continuation .~ continuation)
                nextResponseFold

      where
        bucket :: Bucket
        bucket =
          Bucket (request ^. Proto.type') (request ^. Proto.bucket)

parseSecondaryIndexQueryError ::
     Bucket
  -> ByteString
  -> Maybe (Error 'SecondaryIndexQueryOp)
parseSecondaryIndexQueryError bucket err
  | isInsufficientNodesError err =
      Just InsufficientNodesError
  | isSecondaryIndexesNotSupportedError err =
      Just (SecondaryIndexesNotSupportedError bucket)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))


doIndexPage ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO (Response 26) r
  -> IO (Either HandleError (Either ByteString (r, Maybe ByteString)))
doIndexPage handle request fold =
  Handle.secondaryIndex
    handle
    request
    ((,)
      <$> fold
      <*> continuation)

  where
    continuation :: FoldM IO (Response 26) (Maybe ByteString)
    continuation =
      Foldl.last
        & lmap ((\(RespRpbIndex response) -> response ^. Proto.maybe'continuation))
        & Foldl.generalize
        & fmap join

-- | List all of the keys in a bucket.
--
-- This is 'streamKeys' with a simpler type, but pulls all keys into
-- memory before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.SecondaryIndexQuery.inBucket' query.
--
-- /See also/: 'streamKeys'
listKeys ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either ListKeysError [Key])
listKeys handle bucket =
  streamKeys handle bucket (Foldl.generalize Foldl.list)

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.SecondaryIndexQuery.inBucket' query.
--
-- /See also/: 'listKeys'
streamKeys ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> m (Either ListKeysError r)
streamKeys handle bucket keyFold =
  liftIO (retrying 1000000 (streamKeys_ handle bucket keyFold))

streamKeys_ ::
     Handle
  -> Bucket
  -> FoldM IO Key r
  -> IO (Maybe (Either ListKeysError r))
streamKeys_ handle b@(Bucket bucketType bucket) keyFold =
  doRequest >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseListKeysError bucketType err)

    Right (Right response) ->
      pure (Just (Right response))

  where
    doRequest =
      Handle.listKeys
        handle
        request
        (Foldl.handlesM
          (to (\(RespRpbListKeys response) -> response) . Proto.keys . folded . to (Key bucketType bucket))
          keyFold)

    request :: Proto.RpbListKeysReq
    request =
      Proto.defMessage
        & setProto b
        -- TODO stream keys timeout

parseListKeysError :: ByteString -> ByteString -> Maybe ListKeysError
parseListKeysError bucketType err
  | isBucketTypeDoesNotExistError4 err =
      Just (BucketTypeDoesNotExistError bucketType)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

setProto ::
     ( HasLens' a "bucket" ByteString
     , HasLens' a "maybe'type'" (Maybe ByteString)
     )
  => Bucket
  -> a
  -> a
setProto (Bucket bucketType bucket) proto =
  proto
    & Proto.bucket .~ bucket
    & Proto.maybe'type' .~ (do
        guard (bucketType /= "default")
        pure bucketType)

extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
