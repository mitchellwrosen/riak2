module Riak.Request
  ( Request(..)
  , encode
  ) where

import Riak.Proto

import Data.Bits          (unsafeShiftR)
import Data.ByteString    (ByteString)
import Data.Word          (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (poke)

import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ProtoLens           as Proto


data Request
  = RequestDelete DeleteRequest
  | RequestGet GetRequest
  | RequestGetBucketProperties GetBucketPropertiesRequest
  | RequestGetBucketTypeProperties GetBucketTypePropertiesRequest
  | RequestGetCrdt GetCrdtRequest
  | RequestGetServerInfo GetServerInfoRequest
  | RequestIndex IndexRequest
  | RequestMapReduce MapReduceRequest
  | RequestPing PingRequest
  | RequestPut PutRequest
  | RequestResetBucketProperties ResetBucketPropertiesRequest
  | RequestSetBucketProperties SetBucketPropertiesRequest
  | RequestSetBucketTypeProperties SetBucketTypePropertiesRequest
  | RequestStreamBuckets StreamBucketsRequest
  | RequestStreamKeys StreamKeysRequest
  | RequestUpdateCrdt UpdateCrdtRequest

instance Show Request where
  show :: Request -> String
  show = \case
    RequestDelete                  request -> show request
    RequestGetBucketProperties     request -> show request
    RequestGetBucketTypeProperties request -> show request
    RequestGetCrdt                 request -> show request
    RequestGetServerInfo           request -> show request
    RequestGet                     request -> show request
    RequestIndex                   request -> show request
    RequestMapReduce               request -> show request
    RequestPing                    request -> show request
    RequestPut                     request -> show request
    RequestResetBucketProperties   request -> show request
    RequestSetBucketProperties     request -> show request
    RequestSetBucketTypeProperties request -> show request
    RequestStreamBuckets           request -> show request
    RequestStreamKeys              request -> show request
    RequestUpdateCrdt              request -> show request

encode :: Request -> ByteString
encode = \case
  RequestDelete                  request -> encode' 13 (Proto.encodeMessage request)
  RequestGet                     request -> encode'  9 (Proto.encodeMessage request)
  RequestGetBucketProperties     request -> encode' 19 (Proto.encodeMessage request)
  RequestGetBucketTypeProperties request -> encode' 31 (Proto.encodeMessage request)
  RequestGetCrdt                 request -> encode' 80 (Proto.encodeMessage request)
  RequestGetServerInfo           request -> encode'  7 (Proto.encodeMessage request)
  RequestIndex                   request -> encode' 25 (Proto.encodeMessage request)
  RequestMapReduce               request -> encode' 23 (Proto.encodeMessage request)
  RequestPing                    request -> encode'  1 (Proto.encodeMessage request)
  RequestPut                     request -> encode' 11 (Proto.encodeMessage request)
  RequestResetBucketProperties   request -> encode' 29 (Proto.encodeMessage request)
  RequestSetBucketProperties     request -> encode' 21 (Proto.encodeMessage request)
  RequestSetBucketTypeProperties request -> encode' 31 (Proto.encodeMessage request)
  RequestStreamBuckets           request -> encode' 15 (Proto.encodeMessage request)
  RequestStreamKeys              request -> encode' 17 (Proto.encodeMessage request)
  RequestUpdateCrdt              request -> encode' 82 (Proto.encodeMessage request)

encode' :: Word8 -> ByteString -> ByteString
encode' code request =
  ByteString.unsafeCreate (5 + ByteString.length request) $ \ptr -> do
    pokeWord32BE ptr (fromIntegral (1 + ByteString.length request))
    poke (ptr `plusPtr` 4) code
    pokeByteString (ptr `plusPtr` 5) request

pokeByteString :: Ptr Word8 -> ByteString -> IO ()
pokeByteString dst (ByteString.PS srcf offset len) =
  withForeignPtr srcf $ \src ->
    ByteString.memcpy dst (src `plusPtr` offset) len

pokeWord32BE :: Ptr Word8 -> Word32 -> IO ()
pokeWord32BE ptr word = do
  poke  ptr              (fromIntegral (unsafeShiftR word 24) :: Word8)
  poke (ptr `plusPtr` 1) (fromIntegral (unsafeShiftR word 16) :: Word8)
  poke (ptr `plusPtr` 2) (fromIntegral (unsafeShiftR word  8) :: Word8)
  poke (ptr `plusPtr` 3) (fromIntegral (             word   ) :: Word8)

-- instance Request RpbSearchQueryReq         where code = 27
-- instance Request RpbYokozunaIndexDeleteReq where code = 57
-- instance Request RpbYokozunaIndexGetReq    where code = 54
-- instance Request RpbYokozunaIndexPutReq    where code = 56
-- instance Request RpbYokozunaSchemaGetReq   where code = 58
-- instance Request RpbYokozunaSchemaPutReq   where code = 60
