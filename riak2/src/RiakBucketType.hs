module RiakBucketType where

import Libriak.Response     (Response(..))
import RiakBucket           (Bucket(..))
import RiakBucketProperties (BucketProperties)
import RiakError
import RiakHandle           (Handle)
import RiakIndexName        (IndexName(..))
import RiakUtils            (retrying)

import qualified RiakBucketProperties as BucketProperties
import qualified RiakHandle           as Handle

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, (.~), (^.))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Control.Foldl   as Foldl
import qualified Data.Riak.Proto as Proto


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
type BucketType
  = ByteString

-- | The default bucket type.
defaultBucketType :: BucketType
defaultBucketType =
  "default"

-- | Get bucket type properties.
getBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetBucketTypeError (Maybe BucketProperties))
getBucketType handle bucketType = liftIO $
  Handle.getBucketType handle bucketType >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (parseGetBucketTypeError err)

    Right (Right (RespRpbGetBucket response)) ->
      pure (Right (Just (BucketProperties.fromProto (response ^. Proto.props))))

parseGetBucketTypeError ::
     ByteString
  -> Either GetBucketTypeError (Maybe BucketProperties)
parseGetBucketTypeError err
  | isBucketTypeDoesNotExistError3 err =
      Right Nothing
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Set the index of a bucket type.
--
-- If given the default bucket type, returns 'BucketTypeInvalid', because its
-- properties cannot be changed.
--
-- If the search index's @nodes@ value does not match the bucket's, returns
-- '
-- properties cannot be changed.
--
-- /Note/: If search is not enabled, Riak does not complain if you associate a
-- bucket type with an index that does exist.
--
-- /See also/: 'Riak.Index.putIndex'
setBucketTypeIndex ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> IndexName -- ^ Index name
  -> m (Either SetBucketTypeIndexError ())
setBucketTypeIndex handle bucketType index
  -- Careful changing this code... does it still make sense for
  -- 'unsetBucketTypeIndex' to share an error type?
  | bucketType == defaultBucketType =
      pure (Left (InvalidBucketTypeError bucketType))
  | otherwise =
      setBucketTypeIndex_ handle bucketType index

setBucketTypeIndex_ ::
     MonadIO m
  => Handle
  -> BucketType
  -> IndexName
  -> m (Either SetBucketTypeIndexError ())
setBucketTypeIndex_ handle bucketType index = liftIO $
  Handle.setBucketType handle request >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseSetBucketTypeIndexError bucketType index err))

    Right (Right _) ->
      pure (Right ())

  where
    request :: Proto.RpbSetBucketTypeReq
    request =
      Proto.defMessage
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 (unIndexName index))
        & Proto.type' .~ bucketType

-- | Unset the index of a bucket type.
unsetBucketTypeIndex ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either SetBucketTypeIndexError ())
unsetBucketTypeIndex handle bucketType =
  setBucketTypeIndex handle bucketType (IndexName "_dont_index")

parseSetBucketTypeIndexError ::
     ByteString
  -> IndexName
  -> ByteString
  -> SetBucketTypeIndexError
parseSetBucketTypeIndexError bucketType index err
  | isBucketTypeDoesNotExistError2 err =
      BucketTypeDoesNotExistError bucketType
  | isIndexDoesNotExistError0 err =
      IndexDoesNotExistError index
  | isInvalidNodesError1 err =
      InvalidNodesError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | List all of the buckets in a bucket type.
--
-- This is 'streamBuckets' with a simpler type, but pulls all keys into memory
-- before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'streamBuckets'
listBuckets ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either ListBucketsError [Bucket])
listBuckets handle bucketType =
  streamBuckets handle bucketType (Foldl.generalize Foldl.list)

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'listBuckets'
streamBuckets ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> m (Either ListBucketsError r)
streamBuckets handle bucketType bucketFold =
  liftIO (retrying 1000000 (streamBuckets_ handle bucketType bucketFold))

streamBuckets_ ::
     Handle
  -> BucketType
  -> FoldM IO Bucket r
  -> IO (Maybe (Either ListBucketsError r))
streamBuckets_ handle bucketType bucketFold =
  Handle.listBuckets handle request (makeResponseFold bucketType bucketFold) >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseListBucketsError bucketType err)

    Right (Right response) ->
      pure (Just (Right response))

  where
    request :: Proto.RpbListBucketsReq
    request =
      Proto.defMessage
        & Proto.stream .~ True
        & Proto.type' .~ bucketType
        -- TODO stream buckets timeout

parseListBucketsError :: ByteString -> ByteString -> Maybe ListBucketsError
parseListBucketsError bucketType err
  | isBucketTypeDoesNotExistError4 err =
      Just (BucketTypeDoesNotExistError bucketType)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

makeResponseFold ::
     forall m r. Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m (Response 16) r
makeResponseFold bucketType =
  Foldl.handlesM handler

  where
    handler :: Foldl.HandlerM m (Response 16) Bucket
    handler =
      to (\(RespRpbListBuckets response) -> response) .
      Proto.buckets .
      folded .
      to (Bucket bucketType)
