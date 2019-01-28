module Riak.Response
  ( Response(..)
  , parse
  , DecodeError(..)
  , encode
  ) where

import Riak.Proto

import qualified Utils

import Data.Bifunctor    (bimap)
import Data.ByteString   (ByteString)
import Data.Word         (Word8)

import qualified Data.ByteString as ByteString
import qualified Data.ProtoLens  as Proto


data Response
  = ResponseDelete DeleteResponse
  | ResponseError ErrorResponse
  | ResponseGet GetResponse
  | ResponseGetBucketProperties GetBucketPropertiesResponse
  | ResponseGetCrdt GetCrdtResponse
  | ResponseGetServerInfo GetServerInfoResponse
  | ResponseIndex IndexResponse
  | ResponseListBuckets ListBucketsResponse
  | ResponseListKeys ListKeysResponse
  | ResponseMapReduce MapReduceResponse
  | ResponsePing PingResponse
  | ResponsePut PutResponse
  | ResponseResetBucketProperties ResetBucketPropertiesResponse
  | ResponseSetBucketProperties SetBucketPropertiesResponse
  | ResponseUpdateCrdt UpdateCrdtResponse
  deriving stock (Show)

-- | Parse a response, which consists of a 1-byte message code and a payload.
-- This function assumes the 4-byte, big-endian length prefix has already been
-- stripped.
parse :: ByteString -> Either DecodeError Response
parse bytes =
  decode (ByteString.head bytes) (ByteString.tail bytes)

decode :: Word8 -> ByteString -> Either DecodeError Response
decode code bytes =
  case code of
    0  -> go ResponseError
    2  -> go ResponsePing
    -- 4  -> go ResponseGetClientId
    -- 6  -> go ResponseSetClientId
    8  -> go ResponseGetServerInfo
    10 -> go ResponseGet
    12 -> go ResponsePut
    14 -> go ResponseDelete
    16 -> go ResponseListBuckets
    18 -> go ResponseListKeys
    20 -> go ResponseGetBucketProperties
    22 -> go ResponseSetBucketProperties
    24 -> go ResponseMapReduce
    26 -> go ResponseIndex
    30 -> go ResponseResetBucketProperties
    81 -> go ResponseGetCrdt
    83 -> go ResponseUpdateCrdt

-- instance Response RpbSearchQueryResp       where code = 28
-- instance Response RpbYokozunaIndexGetResp  where code = 55
-- instance Response RpbYokozunaSchemaGetResp where code = 59

    code -> Left (UnknownMessageCode code bytes)

  where
    go ::
         Proto.Message a
      => (a -> Response)
      -> Either DecodeError Response
    go f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)

-- | Encode a response, including the length prefix.
encode :: Response -> ByteString
encode = \case
  ResponseDelete                response -> Utils.wire 14 response
  ResponseError                 response -> Utils.wire 0  response
  ResponseGet                   response -> Utils.wire 10 response
  ResponseGetBucketProperties   response -> Utils.wire 20 response
  ResponseGetCrdt               response -> Utils.wire 81 response
  ResponseGetServerInfo         response -> Utils.wire 8  response
  ResponseIndex                 response -> Utils.wire 26 response
  ResponseListBuckets           response -> Utils.wire 16 response
  ResponseListKeys              response -> Utils.wire 18 response
  ResponseMapReduce             response -> Utils.wire 24 response
  ResponsePing                  response -> Utils.wire 2  response
  ResponsePut                   response -> Utils.wire 12 response
  ResponseResetBucketProperties response -> Utils.wire 30 response
  ResponseSetBucketProperties   response -> Utils.wire 22 response
  ResponseUpdateCrdt            response -> Utils.wire 83 response
