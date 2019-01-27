module Riak.Bucket
  ( Bucket(..)
  , get
  , set
  , reset
  , keys
  , streamKeys
  ) where

import Riak.Internal.Client  (Client, Result)
import Riak.Internal.Key     (Key(..))
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
  => Client -- ^
  -> Bucket -- ^
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

-- | Set bucket properties.
-- TODO better set bucket properties type
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

-- | Reset bucket properties.
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

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'keys'
streamKeys
  :: Client -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
streamKeys client (Bucket type' bucket) keyFold =
  Client.stream
    client
    (RequestStreamKeys request)
    (\case
      ResponseStreamKeys response -> Just response
      _ -> Nothing)
    (view L.done)
    (Foldl.handlesM (L.keys . folded . to (Key type' bucket)) keyFold)

  where
    request :: Proto.StreamKeysRequest
    request =
      defMessage
        & L.type' .~ type'
        & L.bucket .~ bucket
        -- TODO stream keys timeout

-- | List all of the keys in a bucket.
--
-- This is 'streamKeys' with a simpler type, but pulls all keys into memory
-- before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'streamKeys'
keys ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> m (Result [Key])
keys client bucket =
  liftIO (streamKeys client bucket (Foldl.generalize Foldl.list))
