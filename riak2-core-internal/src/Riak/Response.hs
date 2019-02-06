module Riak.Response
  ( Response(..)
  , parse
  , DecodeError(..)
  , encode
  ) where

import Riak.Proto

import qualified Utils

import Data.Bifunctor  (bimap)
import Data.ByteString (ByteString)
import Data.Word       (Word8)

import qualified Data.ByteString as ByteString
import qualified Data.ProtoLens  as Proto


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
  -- | ResponseMapReduce MapReduceResponse
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
parse :: ByteString -> Either DecodeError Response
parse bytes =
  decode (ByteString.head bytes) (ByteString.drop 1 bytes)

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
    -- 24   -> decode' ResponseMapReduce
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

-- | Encode a response, including the length prefix.
encode :: Response -> ByteString
encode = \case
  ResponseDelete         response -> Utils.wire 14 response
  ResponseError          response -> Utils.wire 0  response
  ResponseGet            response -> Utils.wire 10 response
  ResponseGetBucket      response -> Utils.wire 20 response
  ResponseGetCrdt        response -> Utils.wire 81 response
  ResponseGetIndex       response -> Utils.wire 55 response
  ResponseGetServerInfo  response -> Utils.wire 8  response
  ResponseGetSchema      response -> Utils.wire 59 response
  ResponseListBuckets    response -> Utils.wire 16 response
  ResponseListKeys       response -> Utils.wire 18 response
  ResponsePing           response -> Utils.wire 2  response
  ResponsePut            response -> Utils.wire 12 response
  ResponseResetBucket    response -> Utils.wire 30 response
  ResponseSecondaryIndex response -> Utils.wire 26 response
  ResponseSetBucket      response -> Utils.wire 22 response
  ResponseUpdateCrdt     response -> Utils.wire 83 response
