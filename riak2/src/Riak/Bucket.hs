module Riak.Bucket
  ( Bucket(..)
  , get
  ) where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Prelude
import Riak.Proto

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
  -> m (Result BucketProperties)
get client (Bucket type' bucket) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getBucketProperties (iface client) request)

  where
    request :: GetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.type' .~ type'

    fromResponse :: GetBucketPropertiesResponse -> BucketProperties
    fromResponse =
      view L.props
