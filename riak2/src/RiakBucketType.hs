module RiakBucketType
  ( BucketType
  , defaultBucketType
    -- ** Bucket type properties
  , getBucketType
  , setBucketTypeIndex
  , unsetBucketTypeIndex
    -- ** Full traversals
  , listBuckets
  , streamBuckets
  ) where

import Libriak.Handle               (Handle)
import RiakBucket                   (Bucket(..))
import RiakInternalBucketProperties (BucketProperties)
import RiakInternalError
import RiakInternalIndexName        (IndexName(..))
import RiakInternalPrelude

import qualified Libriak.Handle               as Handle
import qualified Libriak.Proto                as Proto
import qualified RiakInternalBucketProperties as BucketProperties

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, (.~))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Control.Foldl as Foldl


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
  fromHandleResult
    parseGetBucketTypeError
    (Just . BucketProperties.fromProto)
    (Handle.getBucketType handle bucketType)

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
  fromHandleResult
    (Left . parseSetBucketTypeIndexError bucketType index)
    id
    (Handle.setBucketType handle request)

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
streamBuckets handle bucketType bucketFold = liftIO $
  fromHandleResult
    (Left . parseListBucketsError bucketType)
    id
    (Handle.listBuckets
      handle
      request
      (makeResponseFold bucketType bucketFold))

  where
    request :: Proto.RpbListBucketsReq
    request =
      Proto.defMessage
        & Proto.stream .~ True
        & Proto.type' .~ bucketType
        -- TODO stream buckets timeout

parseListBucketsError :: ByteString -> ByteString -> ListBucketsError
parseListBucketsError bucketType err
  | isBucketTypeDoesNotExistError4 err =
      BucketTypeDoesNotExistError bucketType
  | otherwise =
      UnknownError (decodeUtf8 err)

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.RpbListBucketsResp r
makeResponseFold bucketType =
  Foldl.handlesM (Proto.buckets . folded . to (Bucket bucketType))
