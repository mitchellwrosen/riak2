module Riak.Request
  ( Request(..)
  , encode
  ) where

import Riak.Proto

import Data.ByteString (ByteString)
import Data.Word       (Word8)

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import qualified Data.ProtoLens             as Proto


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

encode :: Request -> Lazy.ByteString
encode = \case
  RequestDelete                  request -> go 13 request
  RequestGet                     request -> go  9 request
  RequestGetBucketProperties     request -> go 19 request
  RequestGetBucketTypeProperties request -> go 31 request
  RequestGetCrdt                 request -> go 80 request
  RequestGetServerInfo           request -> go  7 request
  RequestIndex                   request -> go 25 request
  RequestMapReduce               request -> go 23 request
  RequestPing                    request -> go  1 request
  RequestPut                     request -> go 11 request
  RequestResetBucketProperties   request -> go 29 request
  RequestSetBucketProperties     request -> go 21 request
  RequestSetBucketTypeProperties request -> go 31 request
  RequestStreamBuckets           request -> go 15 request
  RequestStreamKeys              request -> go 17 request
  RequestUpdateCrdt              request -> go 82 request

  where
    go :: Proto.Message a => Word8 -> a -> Lazy.ByteString
    go code request =
      Builder.toLazyByteString
        (Builder.int32BE (fromIntegral (ByteString.length bytes + 1))
          <> Builder.word8 code
          <> Builder.byteString bytes)
      where
        bytes :: ByteString
        bytes =
          Proto.encodeMessage request

-- instance Request RpbSearchQueryReq         where code = 27
-- instance Request RpbYokozunaIndexDeleteReq where code = 57
-- instance Request RpbYokozunaIndexGetReq    where code = 54
-- instance Request RpbYokozunaIndexPutReq    where code = 56
-- instance Request RpbYokozunaSchemaGetReq   where code = 58
-- instance Request RpbYokozunaSchemaPutReq   where code = 60
