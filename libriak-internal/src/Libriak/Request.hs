module Libriak.Request
  ( Request(..)
  , encodeRequest
  ) where

import Libriak.Proto

import Control.Monad.ST
import Data.ByteString.Internal (ByteString(..))
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.ForeignPtr           (ForeignPtr(..))

import qualified Data.ProtoLens as Proto


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
  | RequestSearch SearchRequest
  | RequestSecondaryIndex SecondaryIndexRequest
  | RequestSetBucket SetBucketRequest
  | RequestSetBucketType SetBucketTypeRequest
  | RequestUpdateCrdt UpdateCrdtRequest
  deriving stock (Show)

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
  RequestSearch         request -> go 27 request
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
