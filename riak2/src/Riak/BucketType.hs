module Riak.BucketType
  ( BucketType(..)
  , get
  , set
  ) where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude
import Riak.Proto
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L


-- | A bucket type.
--
-- /Note/: Must be UTF-8 encoded.
newtype BucketType
  = BucketType
  { type' :: ByteString
  } deriving stock (Eq, Show)
    deriving newtype (Hashable)

-- | Get bucket type properties.
--
-- TODO BucketProps
get ::
     MonadIO m
  => Client
  -> BucketType
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
