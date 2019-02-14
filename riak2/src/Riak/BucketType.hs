module Riak.BucketType
  ( BucketType(..)
    -- ** Bucket type properties
  , getBucketType
  , setBucketTypeIndex
    -- ** Full traversals
  , listBuckets
  , streamBuckets
  ) where

import Libriak.Handle                 (Handle)
import Riak.Bucket                    (Bucket(..))
import Riak.Internal.BucketProperties (BucketProperties)
import Riak.Internal.IndexName        (IndexName(..))
import Riak.Internal.Prelude

import qualified Libriak.Handle                 as Handle
import qualified Libriak.Proto                  as Proto
import qualified Riak.Internal.BucketProperties as BucketProperties

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, (.~))
import Data.Hashable      (Hashable)
import Data.Text.Encoding (encodeUtf8)

import qualified Control.Foldl as Foldl


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
newtype BucketType
  = BucketType ByteString
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

-- | Get bucket type properties.
getBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either Handle.Error BucketProperties)
getBucketType handle (BucketType bucketType) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.getBucketType handle bucketType)

  where
    fromResponse :: Proto.RpbBucketProps -> BucketProperties
    fromResponse =
      BucketProperties.fromProto

-- | Set the index of a bucket type.
setBucketTypeIndex ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> IndexName -- ^ Index name
  -> m (Either Handle.Error ())
setBucketTypeIndex handle (BucketType bucketType) (IndexName index) =
  liftIO (setBucketType handle request)

  where
    request :: Proto.RpbSetBucketTypeReq
    request =
      Proto.defMessage
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 index)
        & Proto.type' .~ bucketType

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either Handle.Error ())
setBucketType handle request =
  Handle.setBucketType handle request

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
  -> m (Either Handle.Error [Bucket])
listBuckets handle bucketType =
  liftIO (streamBuckets handle bucketType (Foldl.generalize Foldl.list))

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'listBuckets'
streamBuckets ::
     Handle -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> IO (Either Handle.Error r)
streamBuckets handle (BucketType bucketType) bucketFold =
  Handle.listBuckets
    handle
    request
    (makeResponseFold bucketType bucketFold)

  where
    request :: Proto.RpbListBucketsReq
    request =
      Proto.defMessage
        & Proto.stream .~ True
        & Proto.type' .~ bucketType
        -- TODO stream buckets timeout

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.RpbListBucketsResp r
makeResponseFold bucketType =
  Foldl.handlesM (Proto.buckets . folded . to (Bucket bucketType))
