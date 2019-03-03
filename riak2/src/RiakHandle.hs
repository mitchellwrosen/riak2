module RiakHandle
  ( Handle(..)
  , HandleConfig(..)
  , EventHandlers(..)
  , HandleError(..)
  , withHandle
  , delete
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

import Libriak.Connection (Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (Response(..))
import RiakManagedBus     (EventHandlers(..), ManagedBus, ManagedBusError(..),
                           managedBusReady, withManagedBus)
import RiakUtils          (difftimeToMicros)

import qualified Libriak.Proto  as Proto
import qualified RiakManagedBus as ManagedBus

import Control.Concurrent.STM (TVar, atomically, readTVar, retry)
import Control.Foldl          (FoldM)
import Control.Lens           ((.~), (^.))
import Data.Time              (NominalDiffTime)
import GHC.Conc               (registerDelay)
import GHC.TypeLits           (KnownNat)


data Handle
  = Handle
  { bus :: !ManagedBus
  , retries :: !Natural
  , handlers :: !EventHandlers
  }

data HandleConfig
  = HandleConfig
  { endpoint :: !Endpoint
    -- | How long to wait for a response from Riak before timing out.
  , timeout :: !NominalDiffTime
    -- | The additional number of times to attempt a request if it results in a
    -- non-Riak error.
    --
    -- This covers a many possible conditions, including:
    --
    -- * A remote reset or remote shutdown due to Riak not being fully
    --   initialized.
    --
    -- * A remote timeout, configurable via @timeout@.
    --
    -- * A protobuf decode error or incorrect message code (very unexpected
    --   failure conditions, but they exist nonetheless).
    --
    -- A sensible, low number like @3@ should be appropriate for persisting
    -- through common, ephemeral network failures, while still giving up
    -- eventually in the case of a catastrophic error, like accidentally
    -- connecting to an endpoint that isn't Riak.
  , retries :: !Natural
  , handlers :: !EventHandlers
  }

data HandleError :: Type where
  -- | A request timed out waiting for a handle to become connected.
  HandleTimeoutError :: HandleError
  -- | A request was attempted the maximum number of times.
  HandleRetryError :: HandleError
  deriving stock (Eq, Show)


-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO a
withHandle HandleConfig { endpoint, handlers, retries, timeout } callback =
  withManagedBus endpoint (difftimeToMicros timeout) handlers $ \bus ->
    callback (Handle bus retries handlers)

delete ::
     Handle -- ^
  -> Proto.RpbDelReq
  -> IO (Either HandleError (Either ByteString (Response 14)))
delete handle request =
  exchange handle (ReqRpbDel request)

deleteIndex ::
     Handle
  -> ByteString
  -> IO (Either HandleError (Either ByteString (Response 14)))
deleteIndex handle name =
  exchange handle (ReqRpbYokozunaIndexDelete request)

  where
    request :: Proto.RpbYokozunaIndexDeleteReq
    request =
      Proto.defMessage
        & Proto.name .~ name

get ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Either HandleError (Either ByteString (Response 10)))
get handle request =
  exchange handle (ReqRpbGet request)

getBucket ::
     Handle -- ^
  -> Proto.RpbGetBucketReq -- ^
  -> IO (Either HandleError (Either ByteString (Response 20)))
getBucket handle request =
  exchange handle (ReqRpbGetBucket request)

getBucketType ::
     Handle -- ^
  -> ByteString -- ^ Bucket type
  -> IO (Either HandleError (Either ByteString (Response 20)))
getBucketType handle bucketType =
  exchange handle (ReqRpbGetBucketType request)

  where
    request :: Proto.RpbGetBucketTypeReq
    request =
      Proto.defMessage
        & Proto.type' .~ bucketType

getCrdt ::
     Handle
  -> Proto.DtFetchReq
  -> IO (Either HandleError (Either ByteString (Response 81)))
getCrdt handle request =
  exchange handle (ReqDtFetch request)

getIndex ::
     Handle
  -> Maybe ByteString
  -> IO (Either HandleError (Either ByteString (Response 55)))
getIndex handle name =
  exchange handle (ReqRpbYokozunaIndexGet request)

  where
    request :: Proto.RpbYokozunaIndexGetReq
    request =
      Proto.defMessage
        & Proto.maybe'name .~ name

getSchema ::
     Handle
  -> ByteString
  -> IO (Either HandleError (Either ByteString (Response 59)))
getSchema handle name =
  exchange handle (ReqRpbYokozunaSchemaGet request)

  where
    request :: Proto.RpbYokozunaSchemaGetReq
    request =
      Proto.defMessage
        & Proto.name .~ name

getServerInfo ::
     Handle
  -> IO (Either HandleError (Either ByteString (Response 8)))
getServerInfo handle =
  exchange handle (ReqRpbGetServerInfo Proto.defMessage)

listBuckets ::
     Handle
  -> Proto.RpbListBucketsReq
  -> FoldM IO (Response 16) r
  -> IO (Either HandleError (Either ByteString r))
listBuckets handle request =
  stream handle (ReqRpbListBuckets request)

listKeys ::
     Handle
  -> Proto.RpbListKeysReq
  -> FoldM IO (Response 18) r
  -> IO (Either HandleError (Either ByteString r))
listKeys handle request =
  stream handle (ReqRpbListKeys request)

mapReduce ::
     Handle
  -> Proto.RpbMapRedReq
  -> FoldM IO (Response 24) r
  -> IO (Either HandleError (Either ByteString r))
mapReduce handle request =
  stream handle (ReqRpbMapRed request)

ping ::
     Handle
  -> IO (Either HandleError (Either ByteString (Response 2)))
ping handle =
  exchange handle (ReqRpbPing Proto.defMessage)

put ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either HandleError (Either ByteString (Response 12)))
put handle request =
  exchange handle (ReqRpbPut request)

putIndex ::
     Handle
  -> Proto.RpbYokozunaIndexPutReq
  -> IO (Either HandleError (Either ByteString (Response 12)))
putIndex handle request =
  exchange handle (ReqRpbYokozunaIndexPut request)

putSchema ::
     Handle
  -> Proto.RpbYokozunaSchema
  -> IO (Either HandleError (Either ByteString (Response 12)))
putSchema handle schema =
  exchange handle (ReqRpbYokozunaSchemaPut request)

  where
    request :: Proto.RpbYokozunaSchemaPutReq
    request =
      Proto.defMessage
        & Proto.schema .~ schema

resetBucket ::
     Handle
  -> Proto.RpbResetBucketReq
  -> IO (Either HandleError (Either ByteString (Response 30)))
resetBucket handle request =
  exchange handle (ReqRpbResetBucket request)

setBucket ::
     Handle
  -> Proto.RpbSetBucketReq
  -> IO (Either HandleError (Either ByteString (Response 22)))
setBucket handle request =
  exchange handle (ReqRpbSetBucket request)

setBucketType ::
     Handle
  -> Proto.RpbSetBucketTypeReq
  -> IO (Either HandleError (Either ByteString (Response 22)))
setBucketType handle request =
  exchange handle (ReqRpbSetBucketType request)

search ::
     Handle
  -> Proto.RpbSearchQueryReq
  -> IO (Either HandleError (Either ByteString (Response 28)))
search handle request =
  exchange handle (ReqRpbSearchQuery request)

secondaryIndex ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO (Response 26) r
  -> IO (Either HandleError (Either ByteString r))
secondaryIndex handle request =
  stream handle (ReqRpbIndex request)

updateCrdt ::
     Handle -- ^
  -> Proto.DtUpdateReq -- ^
  -> IO (Either HandleError (Either ByteString (Response 83)))
updateCrdt handle request =
  exchange handle (ReqDtUpdate request)

-- | Send a request and receive the response (a single message).
--
-- /Throws/: If another prior thread crashed while using this socket, throws
-- 'Control.Exception.BlockedIndefinitelyOnMVar'.
exchange ::
     forall code.
     KnownNat code
  => Handle -- ^
  -> Request code -- ^
  -> IO (Either HandleError (Either ByteString (Response code)))
exchange Handle { bus, retries } request =
  doExchangeOrStream
    retries
    (ManagedBus.exchange bus request)
    (waitForManagedBus bus)
    0

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => Handle -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either HandleError (Either ByteString r))
stream Handle { bus, retries } request responseFold =
  doExchangeOrStream
    retries
    (ManagedBus.stream bus request responseFold)
    (waitForManagedBus bus)
    0

doExchangeOrStream ::
     forall a.
     Natural
  -> IO (Either ManagedBusError (Either (Response 0) a))
  -> IO (Either HandleError ())
  -> Natural
  -> IO (Either HandleError (Either ByteString a))
doExchangeOrStream retries action wait =
  loop False

  where
    loop ::
         -- Have we waited for the handle to become ready at least once? If so,
         -- treat handle-not-ready errors against the retry count (to prevent
         -- continuously resetting the attempts to 0).
         Bool
      -> Natural
      -> IO (Either HandleError (Either ByteString a))
    loop wasReady attempts =
      if attempts > retries
        then
          -- TODO accumulate errors and return them
          pure (Left HandleRetryError)

        else
          action >>= \case
            Left err ->
              case err of
                ManagedBusNotReadyError ->
                  wait >>= \case
                    Left err ->
                      pure (Left err)

                    Right () ->
                      loop True (if wasReady then attempts+1 else attempts)

                ManagedBusConnectionError _ ->
                  wait >>= \case
                    Left err ->
                      pure (Left err)

                    Right () ->
                      loop wasReady (attempts+1)

                -- Weird to treat a decode error (very unexpected) like a connection
                -- error (expected)
                ManagedBusDecodeError _ ->
                  wait >>= \case
                    Left err ->
                      pure (Left err)

                    Right () ->
                      loop wasReady (attempts+1)

                -- Weird to treat an unexpected response error (very unexpected)
                -- like a connection error (expected)
                ManagedBusUnexpectedResponseError ->
                  wait >>= \case
                    Left err ->
                      pure (Left err)

                    Right () ->
                      loop wasReady (attempts+1)

            Right (Left (RespRpbError err)) ->
              pure (Right (Left (err ^. Proto.errmsg)))

            Right (Right response) ->
              pure (Right (Right response))


waitForManagedBus ::
     ManagedBus
  -> IO (Either HandleError ())
waitForManagedBus bus = do
  -- TODO configure connecting wait time
  timeoutVar :: TVar Bool <-
    registerDelay (5*1000*1000)

  atomically $ do
    (readTVar timeoutVar >>= \case
      True -> pure (Left HandleTimeoutError)
      False -> retry)
    <|>
    (Right () <$ managedBusReady bus)
