module Riak.Response
  ( Response(..)
  , parse
  , DecodeError(..)
  ) where

import Riak.Proto

import Control.Exception (Exception)
import Data.Bifunctor    (bimap)
import Data.ByteString   (ByteString)
import Data.Word         (Word8)

import qualified Data.ByteString            as ByteString
import qualified Data.ProtoLens             as Proto


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

instance Show Response where
  show :: Response -> String
  show = \case
    ResponseDelete                response -> show response
    ResponseError                 response -> show response
    ResponseGet                   response -> show response
    ResponseGetBucketProperties   response -> show response
    ResponseGetCrdt               response -> show response
    ResponseGetServerInfo         response -> show response
    ResponseIndex                 response -> show response
    ResponseMapReduce             response -> show response
    ResponsePing                  response -> show response
    ResponsePut                   response -> show response
    ResponseResetBucketProperties response -> show response
    ResponseSetBucketProperties   response -> show response
    ResponseStreamBuckets         response -> show response
    ResponseStreamKeys            response -> show response
    ResponseUpdateCrdt            response -> show response

data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnknownMessageCode !Word8 !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)

parse :: ByteString -> Either DecodeError Response
parse bytes =
  decode (ByteString.head bytes) (ByteString.tail bytes)

decode :: Word8 -> ByteString -> Either DecodeError Response
decode code bytes =
  case code of
    14 -> go bytes ResponseDelete
    0  -> go bytes ResponseError
    20 -> go bytes ResponseGetBucketProperties
    81 -> go bytes ResponseGetCrdt
    10 -> go bytes ResponseGet
    8  -> go bytes ResponseGetServerInfo
    26 -> go bytes ResponseIndex
    24 -> go bytes ResponseMapReduce
    2  -> go bytes ResponsePing
    12 -> go bytes ResponsePut
    30 -> go bytes ResponseResetBucketProperties
    22 -> go bytes ResponseSetBucketProperties
    16 -> go bytes ResponseStreamBuckets
    18 -> go bytes ResponseStreamKeys
    83 -> go bytes ResponseUpdateCrdt

-- instance Response RpbSearchQueryResp       where code = 28
-- instance Response RpbYokozunaIndexGetResp  where code = 55
-- instance Response RpbYokozunaSchemaGetResp where code = 59

    code -> Left (UnknownMessageCode code bytes)

  where
    go ::
         Proto.Message a
      => ByteString
      -> (a -> Response)
      -> Either DecodeError Response
    go bytes f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)
