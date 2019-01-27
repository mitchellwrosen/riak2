module Riak.BucketType
  ( BucketType(..)
  , get
  , set
  , buckets
  , streamBuckets
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.Internal.Client  (Client, Result)
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
  = BucketType
  { type' :: ByteString
  } deriving stock (Eq, Show)
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
  -> m (Result BucketProperties)
get client (BucketType type') = liftIO $
  (fmap.fmap)
    fromResponse
    (Client.exchange
      client
      (RequestGetBucketTypeProperties request)
      (\case
        ResponseGetBucketProperties response -> Just response
        _ -> Nothing))

  where
    request :: GetBucketTypePropertiesRequest
    request =
      defMessage
        & L.type' .~ type'

    fromResponse :: GetBucketPropertiesResponse -> BucketProperties
    fromResponse =
      view L.props

-- | Set bucket type properties.
--
-- /Note/: You may not increase the @hllPrecision@ property.
--
-- TODO better set bucket type properties type
set
  :: Client -- ^
  -> Proto.SetBucketTypePropertiesRequest -- ^
  -> IO (Result Proto.SetBucketPropertiesResponse)
set client request =
  Client.exchange
    client
    (RequestSetBucketTypeProperties request)
    (\case
      ResponseSetBucketProperties response -> Just response
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
  -> IO (Result r)
streamBuckets client (BucketType type') bucketFold =
  Client.stream
    client
    (RequestStreamBuckets request)
    (\case
      ResponseStreamBuckets response -> Just response
      _ -> Nothing)
    (view L.done)
    (makeResponseFold type' bucketFold)

  where
    request :: Proto.StreamBucketsRequest
    request =
      defMessage
        & L.type' .~ type'
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
  -> m (Result [Bucket])
buckets client type' =
  liftIO (streamBuckets client type' (Foldl.generalize Foldl.list))

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.StreamBucketsResponse r
makeResponseFold type' =
  Foldl.handlesM (L.buckets . folded . to (Bucket type'))
