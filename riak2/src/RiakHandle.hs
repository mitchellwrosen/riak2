-- TODO per-request retries setting

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

import RiakBus         (Bus, BusError(..), EventHandlers(..))
import RiakBusPool     (BusPool, createBusPool)
import RiakHandleError (HandleError(..))
import RiakUtils       (difftimeToMicros)

import qualified RiakBus     as Bus
import qualified RiakBusPool as BusPool

import Control.Foldl      (FoldM)
import Control.Lens       ((.~), (^.))
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
    -- | Approximately how long to leave an unused socket connected before
    -- closing it. A background thread will wake every @idleTimeout/2@ seconds
    -- to close idle connections.
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

-- TODO on failure, pick a different connection
withBus ::
     forall a.
     Handle
  -> (Bus -> IO (Either BusError (Either Proto.RpbErrorResp a)))
  -> IO (Either [HandleError] (Either ByteString a))
withBus Handle { pool, retries } action =
  BusPool.withBus pool go

  where
    go :: Bus -> IO (Either [HandleError] (Either ByteString a))
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
                Left BusTimeoutError ->
                  loop (HandleTimeoutError : errs) (attempts+1)

                -- Ignore pipeline error, it wasn't our fault
                Left BusPipelineError ->
                  loop errs attempts

                Left (BusConnectionError err) ->
                  loop (HandleConnectionError err : errs) (attempts+1)

                -- Weird to treat a decode error (very unexpected) like a
                -- connection error (expected)
                Left (BusDecodeError err) ->
                  loop (HandleDecodeError err : errs) (attempts+1)

                Right (Left response) ->
                  pure (Right (Left (response ^. Proto.errmsg)))

                Right (Right response) ->
                  pure (Right (Right response))

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either [HandleError] (Either ByteString ()))
deleteIndex handle name =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.deleteIndex bus request)

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
  withBus handle $ \bus ->
    Bus.get bus request

getBucket ::
     Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetBucketResp))
getBucket handle request =
  withBus handle $ \bus ->
    Bus.getBucket bus request

getBucketType ::
     Handle -- ^
  -> ByteString -- ^ Bucket type
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetBucketResp))
getBucketType handle bucketType =
  withBus handle $ \bus ->
    Bus.getBucketType bus request

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
  withBus handle $ \bus ->
    Bus.getCrdt bus request

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either [HandleError] (Either ByteString Proto.RpbYokozunaIndexGetResp))
getIndex handle name =
  withBus handle $ \bus ->
    Bus.getIndex bus request

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
  withBus handle $ \bus ->
    Bus.getSchema bus request

  where
    request :: Proto.RpbYokozunaSchemaGetReq
    request =
      Proto.defMessage
        & Proto.name .~ name

getServerInfo ::
     Handle
  -> IO (Either [HandleError] (Either ByteString Proto.RpbGetServerInfoResp))
getServerInfo handle =
  withBus handle Bus.getServerInfo

listBuckets ::
     Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO Proto.RpbListBucketsResp r
  -> IO (Either [HandleError] (Either ByteString r))
listBuckets handle request responseFold =
  withBus handle $ \bus ->
    Bus.listBuckets bus request responseFold

listKeys ::
     Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO Proto.RpbListKeysResp r
  -> IO (Either [HandleError] (Either ByteString r))
listKeys handle request responseFold =
  withBus handle $ \bus ->
    Bus.listKeys bus request responseFold

mapReduce ::
     Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO Proto.RpbMapRedResp r
  -> IO (Either [HandleError] (Either ByteString r))
mapReduce handle request responseFold =
  withBus handle $ \bus ->
    Bus.mapReduce bus request responseFold

ping ::
     Handle
  -> IO (Either [HandleError] (Either ByteString ()))
ping handle =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.ping bus)

put ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either [HandleError] (Either ByteString Proto.RpbPutResp))
put handle request =
  withBus handle $ \bus ->
    Bus.put bus request

putIndex ::
     Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either [HandleError] (Either ByteString ()))
putIndex handle request =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.putIndex bus request)

putSchema ::
     Handle
  -> Proto.RpbYokozunaSchema
  -> IO (Either [HandleError] (Either ByteString ()))
putSchema handle schema =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.putSchema bus request)

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
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.resetBucket bus request)

setBucket ::
     Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either [HandleError] (Either ByteString ()))
setBucket handle request =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.setBucket bus request)

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either [HandleError] (Either ByteString ()))
setBucketType handle request =
  withBus handle $ \bus ->
    (fmap.fmap) (() <$)
      (Bus.setBucketType bus request)

search ::
     Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either [HandleError] (Either ByteString Proto.RpbSearchQueryResp))
search handle request =
  withBus handle $ \bus ->
    Bus.search bus request

secondaryIndex ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either [HandleError] (Either ByteString r))
secondaryIndex handle request responseFold =
  withBus handle $ \bus ->
    Bus.secondaryIndex bus request responseFold

updateCrdt ::
     Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either [HandleError] (Either ByteString Proto.DtUpdateResp))
updateCrdt handle request =
  withBus handle $ \bus ->
    Bus.updateCrdt bus request
