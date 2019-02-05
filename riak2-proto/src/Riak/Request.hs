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
  | RequestGet GetRequest
  | RequestGetBucketProperties GetBucketPropertiesRequest
  | RequestGetBucketTypeProperties GetBucketTypePropertiesRequest
  | RequestGetCrdt GetCrdtRequest
  | RequestGetIndex GetIndexRequest
  | RequestGetServerInfo GetServerInfoRequest
  | RequestListBuckets ListBucketsRequest
  | RequestListKeys ListKeysRequest
  -- | RequestMapReduce MapReduceRequest
  | RequestPing PingRequest
  | RequestPut PutRequest
  | RequestResetBucketProperties ResetBucketPropertiesRequest
  | RequestSecondaryIndex SecondaryIndexRequest
  | RequestSetBucketProperties SetBucketPropertiesRequest
  | RequestSetBucketTypeProperties SetBucketTypePropertiesRequest
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
    19   -> go RequestGetBucketProperties
    21   -> go RequestSetBucketProperties
    -- 23   -> go RequestMapReduce
    25   -> go RequestSecondaryIndex
    -- 27   -> go RequestSearchQuery
    29   -> go RequestResetBucketProperties
    31   -> go RequestGetBucketTypeProperties
    32   -> go RequestSetBucketTypeProperties
    -- 33   -> go RequestGetBucketKeyPreflist
    54   -> go RequestGetIndex
    -- 56   -> go RequestPutYokozunaIndex
    -- 57   -> go RequestDeleteYokozunaIndex
    -- 58   -> go RequestGetYokozunaSchema
    -- 60   -> go RequestPutYokozunaSchema
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
  RequestDelete                  request -> Utils.wire 13 request
  RequestGet                     request -> Utils.wire  9 request
  RequestGetBucketProperties     request -> Utils.wire 19 request
  RequestGetBucketTypeProperties request -> Utils.wire 31 request
  RequestGetCrdt                 request -> Utils.wire 80 request
  RequestGetIndex                request -> Utils.wire 54 request
  RequestGetServerInfo           request -> Utils.wire  7 request
  -- RequestMapReduce               request -> Utils.wire 23 request
  RequestPing                    request -> Utils.wire  1 request
  RequestPut                     request -> Utils.wire 11 request
  RequestResetBucketProperties   request -> Utils.wire 29 request
  RequestSecondaryIndex          request -> Utils.wire 25 request
  RequestSetBucketProperties     request -> Utils.wire 21 request
  RequestSetBucketTypeProperties request -> Utils.wire 32 request
  RequestListBuckets             request -> Utils.wire 15 request
  RequestListKeys                request -> Utils.wire 17 request
  RequestUpdateCrdt              request -> Utils.wire 82 request

-- instance Request RpbSearchQueryReq         where code = 27
-- instance Request RpbYokozunaIndexDeleteReq where code = 57
-- instance Request RpbYokozunaIndexPutReq    where code = 56
-- instance Request RpbYokozunaSchemaGetReq   where code = 58
-- instance Request RpbYokozunaSchemaPutReq   where code = 60
