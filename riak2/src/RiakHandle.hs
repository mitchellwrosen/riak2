module RiakHandle
  ( Handle(..)
  , HandleConfig(..)
  , EventHandlers(..)
  , HandleError
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

import Libriak.Connection (ConnectError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (Response(..))
import RiakBus            (EventHandlers(..))
import RiakManagedBus     (ManagedBus, ManagedBusError, withManagedBus)

import qualified Libriak.Proto  as Proto
import qualified RiakManagedBus as ManagedBus

import Control.Foldl (FoldM)
import Control.Lens  ((.~), (^.))
import GHC.TypeLits  (KnownNat)


data Handle
  = Handle
  { bus :: !ManagedBus
  , handlers :: !EventHandlers
  }

data HandleConfig
  = HandleConfig
  { endpoint :: !Endpoint
  , handlers :: !EventHandlers
  }

type HandleError
  = ManagedBusError


-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO a
withHandle HandleConfig { endpoint, handlers } callback =
  withManagedBus endpoint handlers $ \bus ->
    callback (Handle bus handlers)

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
exchange Handle { bus } request =
  (fmap.fmap.first)
    (\(RespRpbError err) -> err ^. Proto.errmsg)
    (ManagedBus.exchange bus request)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => Handle -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either HandleError (Either ByteString r))
stream Handle { bus } request responseFold =
  (fmap.fmap.first)
    (\(RespRpbError err) -> err ^. Proto.errmsg)
    (ManagedBus.stream bus request responseFold)
