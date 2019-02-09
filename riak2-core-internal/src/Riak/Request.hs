module Riak.Request
  ( Request(..)
  , parse
  , encode
  , encodeRequest
  ) where

import Riak.Proto

import qualified Utils

import Control.Monad.ST
import Data.Bifunctor           (bimap)
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.ForeignPtr           (ForeignPtr(..))

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
  | RequestMapReduce MapReduceRequest
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
    23   -> go RequestMapReduce
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
  RequestGetSchema      request -> Utils.wire 58 request
  RequestGetServerInfo  request -> Utils.wire  7 request
  RequestListBuckets    request -> Utils.wire 15 request
  RequestListKeys       request -> Utils.wire 17 request
  RequestMapReduce      request -> Utils.wire 23 request
  RequestPing           request -> Utils.wire  1 request
  RequestPut            request -> Utils.wire 11 request
  RequestPutIndex       request -> Utils.wire 56 request
  RequestPutSchema      request -> Utils.wire 60 request
  RequestResetBucket    request -> Utils.wire 29 request
  RequestSecondaryIndex request -> Utils.wire 25 request
  RequestSetBucket      request -> Utils.wire 21 request
  RequestSetBucketType  request -> Utils.wire 32 request
  RequestUpdateCrdt     request -> Utils.wire 82 request

encodeRequest :: Request -> [ByteArray]
encodeRequest = \case
  RequestDelete         request -> go 13 request
  RequestDeleteIndex    request -> go 57 request
  RequestGet            request -> go  9 request
  RequestGetBucket      request -> go 19 request
  RequestGetBucketType  request -> go 31 request
  RequestGetCrdt        request -> go 80 request
  RequestGetIndex       request -> go 54 request
  RequestGetSchema      request -> go 58 request
  RequestGetServerInfo  request -> go  7 request
  RequestListBuckets    request -> go 15 request
  RequestListKeys       request -> go 17 request
  RequestMapReduce      request -> go 23 request
  RequestPing           request -> go  1 request
  RequestPut            request -> go 11 request
  RequestPutIndex       request -> go 56 request
  RequestPutSchema      request -> go 60 request
  RequestResetBucket    request -> go 29 request
  RequestSecondaryIndex request -> go 25 request
  RequestSetBucket      request -> go 21 request
  RequestSetBucketType  request -> go 32 request
  RequestUpdateCrdt     request -> go 82 request

  where
    go :: Proto.Message a => Word8 -> a -> [ByteArray]
    go code (Proto.encodeMessage -> PS (ForeignPtr addr _) offset len) =
      [ runST makeCodeByteArray
      , runST makeRequestByteArray
      ]
      where
        makeCodeByteArray :: ST s ByteArray
        makeCodeByteArray = do
          bytes <- newByteArray 1
          writeByteArray bytes 0 code
          unsafeFreezeByteArray bytes

        makeRequestByteArray :: ST s ByteArray
        makeRequestByteArray = do
          bytes <- newByteArray len
          copyAddrToByteArray bytes 0 (Addr addr `plusAddr` offset) len
          unsafeFreezeByteArray bytes
