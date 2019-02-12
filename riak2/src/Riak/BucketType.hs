module Riak.BucketType
  ( BucketType(..)
    -- ** Bucket type properties
  , getBucketType
  -- , setBucketType
    -- ** Full traversals
  , listBuckets
  , streamBuckets
  ) where

import Libriak.Handle                 (Handle)
import Riak.Bucket                    (Bucket(..))
import Riak.Internal.BucketProperties (BucketProperties)
import Riak.Internal.Prelude

import qualified Libriak.Handle                 as Handle
import qualified Libriak.Proto                  as Proto
import qualified Libriak.Proto.Lens             as L
import qualified Riak.Internal.BucketProperties as BucketProperties

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, (.~))
import Data.Default.Class (Default(..))
import Data.Hashable      (Hashable)

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
    fromResponse :: Proto.BucketProperties -> BucketProperties
    fromResponse =
      BucketProperties.fromProto

-- | Set bucket type properties.
--
-- /Note/: You may not increase the @hllPrecision@ property.
--
-- TODO better set bucket type properties type
-- TODO don't allow setting n
setBucketType
  :: Handle -- ^
  -> Proto.SetBucketTypeRequest -- ^
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
listBuckets
  :: MonadIO m
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
streamBuckets
  :: Handle -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> IO (Either Handle.Error r)
streamBuckets handle (BucketType bucketType) bucketFold =
  Handle.listBuckets
    handle
    request
    (makeResponseFold bucketType bucketFold)

  where
    request :: Proto.ListBucketsRequest
    request =
      Proto.defMessage
        & L.bucketType .~ bucketType
        & L.stream .~ True
        -- TODO stream buckets timeout

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.ListBucketsResponse r
makeResponseFold bucketType =
  Foldl.handlesM (L.buckets . folded . to (Bucket bucketType))
