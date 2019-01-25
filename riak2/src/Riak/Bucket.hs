module Riak.Bucket
  ( Bucket(..)
  , get
  , set
  , reset
  , stream
  , list
  ) where

import Riak.BucketType       (BucketType(..))
import Riak.Interface        (Result)
import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L

import Control.Foldl (FoldM(..))
import Control.Lens  (folded, to)

import qualified Control.Foldl as Foldl


-- | A bucket type and bucket.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Bucket
  = Bucket
  { type' :: !ByteString
  , bucket :: !ByteString
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | Get bucket properties.
--
-- TODO BucketProps
get ::
     MonadIO m
  => Client
  -> Bucket
  -> m (Result Proto.BucketProperties)
get client (Bucket type' bucket) = liftIO $
  (fmap.fmap)
    fromResponse
    (Client.exchange
      client
      (RequestGetBucketProperties request)
      (\case
        ResponseGetBucketProperties response -> Just response
        _ -> Nothing))

  where
    request :: Proto.GetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.type' .~ type'

    fromResponse :: Proto.GetBucketPropertiesResponse -> Proto.BucketProperties
    fromResponse =
      view L.props

set
  :: Client -- ^
  -> Proto.SetBucketPropertiesRequest -- ^
  -> IO (Result Proto.SetBucketPropertiesResponse)
set client request =
  Client.exchange
    client
    (RequestSetBucketProperties request)
    (\case
      ResponseSetBucketProperties response -> Just response
      _ -> Nothing)

reset ::
     MonadIO m
  => Client
  -> Bucket
  -> m (Result ())
reset client (Bucket type' bucket) = liftIO $
  Client.exchange
    client
    (RequestResetBucketProperties request)
    (\case
      ResponseResetBucketProperties _ -> Just ()
      _ -> Nothing)

  where
    request :: Proto.ResetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.type' .~ type'

stream
  :: Client -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> IO (Result r)
stream client (BucketType type') bucketFold =
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

list
  :: Client -- ^
  -> BucketType -- ^
  -> IO (Result [Bucket])
list client type' =
  stream client type' (Foldl.generalize Foldl.list)

makeResponseFold ::
     Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.StreamBucketsResponse r
makeResponseFold type' =
  Foldl.handlesM (L.buckets . folded . to (Bucket type'))
