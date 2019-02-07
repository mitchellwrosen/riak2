module Riak.BucketType
  ( BucketType(..)
  , getBucketType
  , setBucketType
  , listBuckets
  , streamBuckets
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.Client           (Client)
import Riak.Internal.Prelude

import qualified Riak.Interface  as Interface
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

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

instance Default BucketType where
  def :: BucketType
  def =
    BucketType "default"

-- | Get bucket type properties.
getBucketType ::
     MonadIO m
  => Client -- ^
  -> BucketType -- ^
  -> m (Either ByteString Proto.BucketProperties)
getBucketType client (BucketType bucketType) =
  liftIO (Interface.getBucketType client bucketType)

-- | Set bucket type properties.
--
-- /Note/: You may not increase the @hllPrecision@ property.
--
-- TODO better set bucket type properties type
-- TODO don't allow setting n
setBucketType
  :: Client -- ^
  -> Proto.SetBucketTypeRequest -- ^
  -> IO (Either ByteString ())
setBucketType client request =
  Interface.setBucketType client request

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
  => Client -- ^
  -> BucketType -- ^
  -> m (Either ByteString [Bucket])
listBuckets client bucketType =
  liftIO (streamBuckets client bucketType (Foldl.generalize Foldl.list))

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'listBuckets'
streamBuckets
  :: Client -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> IO (Either ByteString r)
streamBuckets client (BucketType bucketType) bucketFold =
  Interface.listBuckets
    client
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
