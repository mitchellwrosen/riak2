module Riak.Response
  ( Response(..)
  , parse
  , DecodeError(..)
  , encode
  ) where

import Riak.Proto

import qualified Utils

import Control.Exception (Exception)
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
  | ResponseMapReduce MapReduceResponse
  | ResponsePing PingResponse
  | ResponsePut PutResponse
  | ResponseResetBucketProperties ResetBucketPropertiesResponse
  | ResponseSetBucketProperties SetBucketPropertiesResponse
  | ResponseStreamBuckets StreamBucketsResponse
  | ResponseStreamKeys StreamKeysResponse
  | ResponseUpdateCrdt UpdateCrdtResponse
  deriving stock (Show)

data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnknownMessageCode !Word8 !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse a 'Response', which consists of a 1-byte message code and a payload.
-- This function assumes the 4-byte, big-endian length prefix has already been
-- stripped.
parse :: ByteString -> Either DecodeError Response
parse bytes =
  decode (ByteString.head bytes) (ByteString.tail bytes)

decode :: Word8 -> ByteString -> Either DecodeError Response
decode code bytes =
  case code of
    14   -> go ResponseDelete
    0    -> go ResponseError
    20   -> go ResponseGetBucketProperties
    81   -> go ResponseGetCrdt
    10   -> go ResponseGet
    8    -> go ResponseGetServerInfo
    26   -> go ResponseIndex
    24   -> go ResponseMapReduce
    2    -> go ResponsePing
    12   -> go ResponsePut
    30   -> go ResponseResetBucketProperties
    22   -> go ResponseSetBucketProperties
    16   -> go ResponseStreamBuckets
    18   -> go ResponseStreamKeys
    83   -> go ResponseUpdateCrdt

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
  ResponseGet                   response -> Utils.wire 20 response
  ResponseGetBucketProperties   response -> Utils.wire 81 response
  ResponseGetCrdt               response -> Utils.wire 10 response
  ResponseGetServerInfo         response -> Utils.wire 8  response
  ResponseIndex                 response -> Utils.wire 26 response
  ResponseMapReduce             response -> Utils.wire 24 response
  ResponsePing                  response -> Utils.wire 2  response
  ResponsePut                   response -> Utils.wire 12 response
  ResponseResetBucketProperties response -> Utils.wire 30 response
  ResponseSetBucketProperties   response -> Utils.wire 22 response
  ResponseStreamBuckets         response -> Utils.wire 16 response
  ResponseStreamKeys            response -> Utils.wire 18 response
  ResponseUpdateCrdt            response -> Utils.wire 83 response
