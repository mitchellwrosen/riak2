module RiakHandle
  ( Handle(..)
  , HandleConfig(..)
  , EventHandlers(..)
  , createHandle
  , deleteIndex
  , get
  , getBucket
  , getBucketType
  , getCrdt
  , getIndex
  , getSchema
  , getServerInfo
  , listBuckets
  , listKeys
  , mapReduce
  , ping
  , put
  , putIndex
  , putSchema
  , resetBucket
  , search
  , secondaryIndex
  , setBucket
  , setBucketType
  , updateCrdt
  ) where

import RiakBusPool     (BusPool, createBusPool)
import RiakHandleError (HandleError(..))
import RiakManagedBus  (EventHandlers(..), ManagedBus, ManagedBusError(..))
import RiakUtils       (difftimeToMicros)

import qualified RiakBusPool    as BusPool
import qualified RiakManagedBus as ManagedBus

import Control.Foldl      (FoldM)
import Control.Lens       ((.~))
import Data.Time          (NominalDiffTime)
import Socket.Stream.IPv4 (Endpoint)

import qualified Data.Riak.Proto as Proto


data Handle
  = Handle
  { pool :: BusPool
  , retries :: Natural
  , handlers :: EventHandlers
  }

data HandleConfig
  = HandleConfig
  { endpoint :: Endpoint
    -- | How often to perform a health check on each underlying connection. Use
    -- @0@ to disable.
  , healthCheckInterval :: NominalDiffTime
  , idleTimeout :: NominalDiffTime
    -- | How long to wait for a successful response from Riak before timing out.
    -- This includes the time spent waiting for a successful connection to be
    -- established, time spent waiting for Riak to become healthy, and time
    -- spent waiting for Riak to respond.
  , requestTimeout :: NominalDiffTime
    -- | TODO document connectTimeout
  , connectTimeout :: NominalDiffTime
    -- | The additional number of times to attempt a request if it results in a
    -- non-Riak error.
    --
    -- This covers a many possible conditions, including:
    --
    -- * A remote reset or remote shutdown due to Riak restarting.
    --
    -- * A remote timeout, configurable via @timeout@.
    --
    -- * A protobuf decode error or incorrect message code (very unexpected
    --   failure conditions, but they exist nonetheless).
    --
    -- A low number like @3@ should be appropriate for persisting through
    -- common, ephemeral network failures, while still giving up eventually in
    -- the case of a catastrophic error, like accidentally connecting to an
    -- endpoint that isn't Riak.
  , retries :: Natural
  , handlers :: EventHandlers
  }

-- | Create a handle.
--
-- /Throws/: This function will never throw an exception.
createHandle ::
     HandleConfig
  -> IO Handle
createHandle
    HandleConfig { connectTimeout, endpoint, handlers, healthCheckInterval,
                   idleTimeout, retries, requestTimeout } = do

  pool :: BusPool <-
    createBusPool
      endpoint
      (difftimeToMicros healthCheckInterval)
      (difftimeToMicros idleTimeout)
      (difftimeToMicros requestTimeout)
      (difftimeToMicros connectTimeout)
      handlers

  pure Handle
    { pool = pool
    , retries = retries
    , handlers = handlers
    }

withManagedBus ::
     forall a.
     Handle
  -> (ManagedBus -> IO (Either ManagedBusError (Either ByteString a)))
  -> IO (Either [HandleError] (Either ByteString a))
withManagedBus Handle { pool, retries } action =
  BusPool.withManagedBus pool go

  where
    go :: ManagedBus -> IO (Either [HandleError] (Either ByteString a))
    go bus =
      loop [] 0

      where
        loop ::
             [HandleError]
          -> Natural
          -> IO (Either [HandleError] (Either ByteString a))
        loop errs attempts =
          if attempts > retries
            then
              pure (Left (reverse errs))

            else
              action bus >>= \case
                Left ManagedBusTimeoutError ->
                  loop (HandleTimeoutError : errs) (attempts+1)

                -- Ignore pipeline error, it wasn't our fault
                Left ManagedBusPipelineError ->
                  loop errs attempts

                Left (ManagedBusConnectionError err) ->
                  loop (HandleConnectionError err : errs) (attempts+1)

                -- Weird to treat a decode error (very unexpected) like a
                -- connection error (expected)
                Left (ManagedBusDecodeError err) ->
                  loop (HandleDecodeError err : errs) (attempts+1)

                Right response ->
                  pure (Right response)

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either [HandleError] (Either ByteString ()))
deleteIndex handle name =
  withManagedBus handle $ \bus ->
    ManagedBus.deleteIndex bus request

  where
    request :: Proto.RpbYokozunaIndexDeleteReq
    request =
      Proto.defMessage
        & Proto.name .~ name

get ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetResp))
get handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.get bus request

getBucket ::
     Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetBucketResp))
getBucket handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.getBucket bus request

getBucketType ::
     Handle -- ^
  -> ByteString -- ^ Bucket type
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetBucketResp))
getBucketType handle bucketType =
  withManagedBus handle $ \bus ->
    ManagedBus.getBucketType bus request

  where
    request :: Proto.RpbGetBucketTypeReq
    request =
      Proto.defMessage
        & Proto.type' .~ bucketType

getCrdt ::
     Handle
  -> Proto.DtFetchReq
  -> IO (Either [HandleError] (Either ByteString Proto.DtFetchResp))
getCrdt handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.getCrdt bus request

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either [HandleError] (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex handle name =
  withManagedBus handle $ \bus ->
    ManagedBus.getIndex bus request

  where
    request :: Proto.RpbYokozunaIndexGetReq
    request =
      Proto.defMessage
        & Proto.maybe'name .~ name

getSchema ::
     Handle
  -> ByteString
  -> IO (Either [HandleError] (Either ByteString Proto.RpbYokozunaSchemaGetResp))
getSchema handle name =
  withManagedBus handle $ \bus ->
    ManagedBus.getSchema bus request

  where
    request :: Proto.RpbYokozunaSchemaGetReq
    request =
      Proto.defMessage
        & Proto.name .~ name

getServerInfo ::
     Handle
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo handle =
  withManagedBus handle ManagedBus.getServerInfo

listBuckets ::
     Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either [HandleError] (Either ByteString r))
listBuckets handle request responseFold =
  withManagedBus handle $ \bus ->
    ManagedBus.listBuckets bus request responseFold

listKeys ::
     Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either [HandleError] (Either ByteString r))
listKeys handle request responseFold =
  withManagedBus handle $ \bus ->
    ManagedBus.listKeys bus request responseFold

mapReduce ::
     Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either [HandleError] (Either ByteString r))
mapReduce handle request responseFold =
  withManagedBus handle $ \bus ->
    ManagedBus.mapReduce bus request responseFold

ping ::
     Handle
  -> IO (Either [HandleError] (Either ByteString ()))
ping handle =
  withManagedBus handle ManagedBus.ping

put ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either [HandleError] (Either ByteString Proto.RpbPutResp))
put handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.put bus request

putIndex ::
     Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either [HandleError] (Either ByteString ()))
putIndex handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.putIndex bus request

putSchema ::
     Handle
  -> Proto.RpbYokozunaSchema
  -> IO (Either [HandleError] (Either ByteString ()))
putSchema handle schema =
  withManagedBus handle $ \bus ->
    ManagedBus.putSchema bus request

  where
    request :: Proto.RpbYokozunaSchemaPutReq
    request =
      Proto.defMessage
        & Proto.schema .~ schema

resetBucket ::
     Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either [HandleError] (Either ByteString ()))
resetBucket handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.resetBucket bus request

setBucket ::
     Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either [HandleError] (Either ByteString ()))
setBucket handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.setBucket bus request

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either [HandleError] (Either ByteString ()))
setBucketType handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.setBucketType bus request

search ::
     Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either [HandleError] (Either ByteString Proto.RpbSearchQueryResp))
search handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.search bus request

secondaryIndex ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either [HandleError] (Either ByteString r))
secondaryIndex handle request responseFold =
  withManagedBus handle $ \bus ->
    ManagedBus.secondaryIndex bus request responseFold

updateCrdt ::
     Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either [HandleError] (Either ByteString Proto.DtUpdateResp))
updateCrdt handle request =
  withManagedBus handle $ \bus ->
    ManagedBus.updateCrdt bus request
