module Libriak.Request
  ( Request(..)
  , EncodedRequest(..)
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


-- TODO request 33 is of riak_kv so handle unknown message code by retrying
data Request
  = ReqDtFetch DtFetchReq
  | ReqDtUpdate DtUpdateReq
  | ReqRpbDel RpbDelReq
  | ReqRpbGet RpbGetReq
  | ReqRpbGetBucket RpbGetBucketReq
  | ReqRpbGetBucketType RpbGetBucketTypeReq
  | ReqRpbGetServerInfo RpbGetServerInfoReq
  | ReqRpbIndex RpbIndexReq
  | ReqRpbListBuckets RpbListBucketsReq
  | ReqRpbListKeys RpbListKeysReq
  | ReqRpbMapRed RpbMapRedReq
  | ReqRpbPing RpbPingReq
  | ReqRpbPut RpbPutReq
  | ReqRpbResetBucket RpbResetBucketReq
  | ReqRpbSearchQuery RpbSearchQueryReq
  | ReqRpbSetBucket RpbSetBucketReq
  | ReqRpbSetBucketType RpbSetBucketTypeReq
  | ReqRpbYokozunaIndexDelete RpbYokozunaIndexDeleteReq
  | ReqRpbYokozunaIndexGet RpbYokozunaIndexGetReq
  | ReqRpbYokozunaIndexPut RpbYokozunaIndexPutReq
  | ReqRpbYokozunaSchemaGet RpbYokozunaSchemaGetReq
  | ReqRpbYokozunaSchemaPut RpbYokozunaSchemaPutReq
  deriving stock (Show)

newtype EncodedRequest
  = EncodedRequest { unEncodedRequest :: [ByteArray] }

encodeRequest :: Request -> EncodedRequest
encodeRequest = \case
  ReqDtFetch                request -> go 80 request
  ReqDtUpdate               request -> go 82 request
  ReqRpbDel                 request -> go 13 request
  ReqRpbGet                 request -> go  9 request
  ReqRpbGetBucket           request -> go 19 request
  ReqRpbGetBucketType       request -> go 31 request
  ReqRpbGetServerInfo       request -> go  7 request
  ReqRpbIndex               request -> go 25 request
  ReqRpbListBuckets         request -> go 15 request
  ReqRpbListKeys            request -> go 17 request
  ReqRpbMapRed              request -> go 23 request
  ReqRpbPing                request -> go  1 request
  ReqRpbPut                 request -> go 11 request
  ReqRpbResetBucket         request -> go 29 request
  ReqRpbSearchQuery         request -> go 27 request
  ReqRpbSetBucket           request -> go 21 request
  ReqRpbSetBucketType       request -> go 32 request
  ReqRpbYokozunaIndexDelete request -> go 57 request
  ReqRpbYokozunaIndexGet    request -> go 54 request
  ReqRpbYokozunaIndexPut    request -> go 56 request
  ReqRpbYokozunaSchemaGet   request -> go 58 request
  ReqRpbYokozunaSchemaPut   request -> go 60 request

  where
    go :: Proto.Message a => Word8 -> a -> EncodedRequest
    go code (Proto.encodeMessage -> PS (ForeignPtr addr _) offset len) =
      EncodedRequest
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
