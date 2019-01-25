module Riak.Bucket
  ( Bucket(..)
  , get
  , reset
  ) where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Prelude

import qualified Riak.Proto as Proto
import qualified Riak.Interface  as Interface
import qualified Riak.Proto.Lens as L


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
    (Interface.getBucketProperties (iface client) request)

  where
    request :: Proto.GetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.type' .~ type'

    fromResponse :: Proto.GetBucketPropertiesResponse -> Proto.BucketProperties
    fromResponse =
      view L.props

reset ::
     MonadIO m
  => Client
  -> Bucket
  -> m (Result ())
reset client (Bucket type' bucket) = liftIO $
  Interface.resetBucketProperties (iface client) request

  where
    request :: Proto.ResetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.type' .~ type'
