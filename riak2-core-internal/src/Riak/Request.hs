module Riak.Request
  ( Request(..)
  , parse
  , encode
  ) where

import Riak.Proto

import qualified Utils

import Data.Bifunctor  (bimap)
import Data.ByteString (ByteString)
import Data.Word       (Word8)

import qualified Data.ByteString as ByteString
import qualified Data.ProtoLens  as Proto


data Request
  = RequestDelete DeleteRequest
  | RequestDeleteIndex DeleteIndexRequest
  | RequestGet GetRequest
  | RequestGetBucket GetBucketRequest
  | RequestGetBucketType GetBucketTypeRequest
  | RequestGetCrdt GetCrdtRequest
  | RequestGetIndex GetIndexRequest
  | RequestGetServerInfo GetServerInfoRequest
  | RequestGetSchema GetSchemaRequest
  | RequestListBuckets ListBucketsRequest
  | RequestListKeys ListKeysRequest
  -- | RequestMapReduce MapReduceRequest
  | RequestPing PingRequest
  | RequestPut PutRequest
  | RequestPutIndex PutIndexRequest
  | RequestPutSchema PutSchemaRequest
  | RequestResetBucket ResetBucketRequest
  | RequestSecondaryIndex SecondaryIndexRequest
  | RequestSetBucket SetBucketRequest
  | RequestSetBucketType SetBucketTypeRequest
  | RequestUpdateCrdt UpdateCrdtRequest
  deriving stock (Show)

-- | Parse a request, which consists of a 1-byte message code and a payload.
-- This function assumes the 4-byte, big-endian length prefix has already been
-- stripped.
parse :: ByteString -> Either DecodeError Request
parse bytes =
  decode (ByteString.head bytes) (ByteString.tail bytes)

decode :: Word8 -> ByteString -> Either DecodeError Request
decode code bytes =
  case code of
    1    -> go RequestPing
    -- 3    -> go RequestGetClientId
    -- 5    -> go RequestSetClientId
    7    -> go RequestGetServerInfo
    9    -> go RequestGet
    11   -> go RequestPut
    13   -> go RequestDelete
    15   -> go RequestListBuckets
    17   -> go RequestListKeys
    19   -> go RequestGetBucket
    21   -> go RequestSetBucket
    -- 23   -> go RequestMapReduce
    25   -> go RequestSecondaryIndex
    -- 27   -> go RequestSearchQuery
    29   -> go RequestResetBucket
    31   -> go RequestGetBucketType
    32   -> go RequestSetBucketType
    -- 33   -> go RequestGetBucketKeyPreflist
    54   -> go RequestGetIndex
    56   -> go RequestPutIndex
    57   -> go RequestDeleteIndex
    58   -> go RequestGetSchema
    60   -> go RequestPutSchema
    -- 70   -> go RequestCoverage
    80   -> go RequestGetCrdt
    82   -> go RequestUpdateCrdt

    _    -> Left (UnknownMessageCode code bytes)

  where
    go ::
         Proto.Message a
      => (a -> Request)
      -> Either DecodeError Request
    go f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)

encode :: Request -> ByteString
encode = \case
  RequestDelete         request -> Utils.wire 13 request
  RequestDeleteIndex    request -> Utils.wire 57 request
  RequestGet            request -> Utils.wire  9 request
  RequestGetBucket      request -> Utils.wire 19 request
  RequestGetBucketType  request -> Utils.wire 31 request
  RequestGetCrdt        request -> Utils.wire 80 request
  RequestGetIndex       request -> Utils.wire 54 request
  RequestGetServerInfo  request -> Utils.wire  7 request
  RequestGetSchema      request -> Utils.wire 58 request
  RequestPing           request -> Utils.wire  1 request
  RequestPut            request -> Utils.wire 11 request
  RequestPutIndex       request -> Utils.wire 56 request
  RequestPutSchema      request -> Utils.wire 60 request
  RequestResetBucket    request -> Utils.wire 29 request
  RequestSecondaryIndex request -> Utils.wire 25 request
  RequestSetBucket      request -> Utils.wire 21 request
  RequestSetBucketType  request -> Utils.wire 32 request
  RequestListBuckets    request -> Utils.wire 15 request
  RequestListKeys       request -> Utils.wire 17 request
  RequestUpdateCrdt     request -> Utils.wire 82 request
