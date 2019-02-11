module Libriak.Response
  ( Response(..)
  , parseResponse
  , DecodeError(..)
  ) where

import Libriak.Proto

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bifunctor           (bimap)
import Data.ByteString          (ByteString)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.Ptr                  (Ptr(..))

import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ProtoLens           as Proto


data Response
  = ResponseDelete DeleteResponse
  | ResponseError ErrorResponse
  | ResponseGet GetResponse
  | ResponseGetBucket GetBucketResponse
  | ResponseGetCrdt GetCrdtResponse
  | ResponseGetIndex GetIndexResponse
  | ResponseGetServerInfo GetServerInfoResponse
  | ResponseGetSchema GetSchemaResponse
  | ResponseListBuckets ListBucketsResponse
  | ResponseListKeys ListKeysResponse
  | ResponseMapReduce MapReduceResponse
  | ResponsePing PingResponse
  | ResponsePut PutResponse
  | ResponseResetBucket ResetBucketResponse
  | ResponseSecondaryIndex SecondaryIndexResponse
  | ResponseSetBucket SetBucketResponse
  | ResponseUpdateCrdt UpdateCrdtResponse
  deriving stock (Show)

-- | Parse a response, which consists of a 1-byte message code and a payload.
-- This function assumes the 4-byte, big-endian length prefix has already been
-- stripped.
parseResponse :: ByteArray -> Either DecodeError Response
parseResponse bytes =
  decode (indexByteArray bytes 0) (runST makeByteStringFromPayload)
  where
    makeByteStringFromPayload :: ST s ByteString
    makeByteStringFromPayload =
      unsafeIOToST
        (ByteString.create
          payloadLen
          (\(Ptr addr) -> copyByteArrayToAddr (Addr addr) bytes 1 payloadLen))

    payloadLen :: Int
    payloadLen =
      sizeofByteArray bytes - 1

decode :: Word8 -> ByteString -> Either DecodeError Response
decode code bytes =
  case code of
    0    -> decode' ResponseError
    2    -> Right (ResponsePing Proto.defMessage)
    8    -> decode' ResponseGetServerInfo
    10   -> decode' ResponseGet
    12   -> decode' ResponsePut
    14   -> Right (ResponseDelete Proto.defMessage)
    16   -> decode' ResponseListBuckets
    18   -> decode' ResponseListKeys
    20   -> decode' ResponseGetBucket
    22   -> Right (ResponseSetBucket Proto.defMessage)
    24   -> decode' ResponseMapReduce
    26   -> decode' ResponseSecondaryIndex
    -- 28   -> decode' ResponseSearch
    30   -> Right (ResponseResetBucket Proto.defMessage)
    55   -> decode' ResponseGetIndex
    59   -> decode' ResponseGetSchema
    81   -> decode' ResponseGetCrdt
    83   -> decode' ResponseUpdateCrdt
    _    -> Left (UnknownMessageCode code bytes)

  where
    decode' ::
         Proto.Message a
      => (a -> Response)
      -> Either DecodeError Response
    decode' f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)
