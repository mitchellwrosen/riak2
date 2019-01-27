module Riak.Request
  ( Request(..)
  , encode
  ) where

import Riak.Proto

import qualified Utils

import Data.ByteString (ByteString)


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
  deriving stock (Show)

encode :: Request -> ByteString
encode = \case
  RequestDelete                  request -> Utils.wire 13 request
  RequestGet                     request -> Utils.wire  9 request
  RequestGetBucketProperties     request -> Utils.wire 19 request
  RequestGetBucketTypeProperties request -> Utils.wire 31 request
  RequestGetCrdt                 request -> Utils.wire 80 request
  RequestGetServerInfo           request -> Utils.wire  7 request
  RequestIndex                   request -> Utils.wire 25 request
  RequestMapReduce               request -> Utils.wire 23 request
  RequestPing                    request -> Utils.wire  1 request
  RequestPut                     request -> Utils.wire 11 request
  RequestResetBucketProperties   request -> Utils.wire 29 request
  RequestSetBucketProperties     request -> Utils.wire 21 request
  RequestSetBucketTypeProperties request -> Utils.wire 31 request
  RequestStreamBuckets           request -> Utils.wire 15 request
  RequestStreamKeys              request -> Utils.wire 17 request
  RequestUpdateCrdt              request -> Utils.wire 82 request

-- instance Request RpbSearchQueryReq         where code = 27
-- instance Request RpbYokozunaIndexDeleteReq where code = 57
-- instance Request RpbYokozunaIndexGetReq    where code = 54
-- instance Request RpbYokozunaIndexPutReq    where code = 56
-- instance Request RpbYokozunaSchemaGetReq   where code = 58
-- instance Request RpbYokozunaSchemaPutReq   where code = 60
