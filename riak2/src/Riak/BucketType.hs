module Riak.BucketType
  ( BucketType(..)
  , get
  , set
  , buckets
  , streamBuckets
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude
import Riak.Proto
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L

import Control.Foldl (FoldM(..))
import Control.Lens  (folded, to)

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
--
-- TODO BucketProps
get ::
     MonadIO m
  => Client -- ^
  -> BucketType -- ^
  -> m (Either Text BucketProperties)
get client (BucketType bucketType) = liftIO $
  (fmap.fmap)
    fromResponse
    (Client.exchange
      client
      (RequestGetBucketType request)
      (\case
        ResponseGetBucket response -> Just response
        _ -> Nothing))

  where
    request :: GetBucketTypeRequest
    request =
      defMessage
        & L.bucketType .~ bucketType

    fromResponse :: GetBucketResponse -> BucketProperties
    fromResponse =
      view L.props

-- | Set bucket type properties.
--
-- /Note/: You may not increase the @hllPrecision@ property.
--
-- TODO better set bucket type properties type
-- TODO don't allow setting n
set
  :: Client -- ^
  -> Proto.SetBucketTypeRequest -- ^
  -> IO (Either Text Proto.SetBucketResponse)
set client request =
  Client.exchange
    client
    (RequestSetBucketType request)
    (\case
      ResponseSetBucket response -> Just response
      _ -> Nothing)

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'buckets'
streamBuckets
  :: Client -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> IO (Either Text r)
streamBuckets client (BucketType bucketType) bucketFold =
  Client.stream
    client
    (RequestListBuckets request)
    (\case
      ResponseListBuckets response -> Just response
      _ -> Nothing)
    (view L.done)
    (makeResponseFold bucketType bucketFold)

  where
    request :: Proto.ListBucketsRequest
    request =
      defMessage
        & L.bucketType .~ bucketType
        & L.stream .~ True
        -- TODO stream buckets timeout

-- | List all of the buckets in a bucket type.
--
-- This is 'streamBuckets' with a simpler type, but pulls all keys into memory
-- before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'streamBuckets'
buckets
  :: MonadIO m
  => Client -- ^
  -> BucketType -- ^
  -> m (Either Text [Bucket])
buckets client bucketType =
  liftIO (streamBuckets client bucketType (Foldl.generalize Foldl.list))

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.ListBucketsResponse r
makeResponseFold bucketType =
  Foldl.handlesM (L.buckets . folded . to (Bucket bucketType))
