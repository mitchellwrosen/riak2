module Libriak.Request
  ( Request(..)
  , EncodedRequest(..)
  , encodeDtFetch
  , encodeDtUpdate
  , encodeRpbDel
  , encodeRpbGet
  , encodeRpbGetBucket
  , encodeRpbGetBucketType
  , encodeRpbGetServerInfo
  , encodeRpbIndex
  , encodeRpbListBuckets
  , encodeRpbListKeys
  , encodeRpbMapRed
  , encodeRpbPing
  , encodeRpbPut
  , encodeRpbResetBucket
  , encodeRpbSearchQuery
  , encodeRpbSetBucket
  , encodeRpbSetBucketType
  , encodeRpbYokozunaIndexDelete
  , encodeRpbYokozunaIndexGet
  , encodeRpbYokozunaIndexPut
  , encodeRpbYokozunaSchemaGet
  , encodeRpbYokozunaSchemaPut
  ) where

import Control.DeepSeq          (NFData(..))
import Control.Monad.ST
import Data.ByteString.Internal (ByteString(..))
import Data.Kind                (Type)
import Data.List                (foldl')
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.ForeignPtr           (ForeignPtr(..))

import qualified Data.ProtoLens  as Proto
import qualified Data.Riak.Proto as Proto


-- TODO request 33 is of riak_kv so handle unknown message code by retrying

data Request :: Type where
  ReqDtFetch                :: Proto.DtFetchReq                -> Request
  ReqDtUpdate               :: Proto.DtUpdateReq               -> Request
  ReqRpbDel                 :: Proto.RpbDelReq                 -> Request
  ReqRpbGet                 :: Proto.RpbGetReq                 -> Request
  ReqRpbGetBucket           :: Proto.RpbGetBucketReq           -> Request
  ReqRpbGetBucketType       :: Proto.RpbGetBucketTypeReq       -> Request
  ReqRpbGetServerInfo       :: Proto.RpbGetServerInfoReq       -> Request
  ReqRpbIndex               :: Proto.RpbIndexReq               -> Request
  ReqRpbListBuckets         :: Proto.RpbListBucketsReq         -> Request
  ReqRpbListKeys            :: Proto.RpbListKeysReq            -> Request
  ReqRpbMapRed              :: Proto.RpbMapRedReq              -> Request
  ReqRpbPing                :: Proto.RpbPingReq                -> Request
  ReqRpbPut                 :: Proto.RpbPutReq                 -> Request
  ReqRpbResetBucket         :: Proto.RpbResetBucketReq         -> Request
  ReqRpbSearchQuery         :: Proto.RpbSearchQueryReq         -> Request
  ReqRpbSetBucket           :: Proto.RpbSetBucketReq           -> Request
  ReqRpbSetBucketType       :: Proto.RpbSetBucketTypeReq       -> Request
  ReqRpbYokozunaIndexDelete :: Proto.RpbYokozunaIndexDeleteReq -> Request
  ReqRpbYokozunaIndexGet    :: Proto.RpbYokozunaIndexGetReq    -> Request
  ReqRpbYokozunaIndexPut    :: Proto.RpbYokozunaIndexPutReq    -> Request
  ReqRpbYokozunaSchemaGet   :: Proto.RpbYokozunaSchemaGetReq   -> Request
  ReqRpbYokozunaSchemaPut   :: Proto.RpbYokozunaSchemaPutReq   -> Request
  deriving stock (Eq, Show)

newtype EncodedRequest
  = EncodedRequest { unEncodedRequest :: [ByteArray] }

instance NFData EncodedRequest where
  rnf (EncodedRequest xs) =
    foldl' (\acc x -> x `seq` acc) () xs

encodeDtFetch :: Proto.DtFetchReq -> EncodedRequest
encodeDtFetch =
  encode 80

encodeDtUpdate :: Proto.DtUpdateReq -> EncodedRequest
encodeDtUpdate =
  encode 82

encodeRpbDel :: Proto.RpbDelReq -> EncodedRequest
encodeRpbDel =
  encode 13

encodeRpbGet :: Proto.RpbGetReq -> EncodedRequest
encodeRpbGet =
  encode 9

encodeRpbGetBucket :: Proto.RpbGetBucketReq -> EncodedRequest
encodeRpbGetBucket =
  encode 19

encodeRpbGetBucketType :: Proto.RpbGetBucketTypeReq -> EncodedRequest
encodeRpbGetBucketType =
  encode 31

encodeRpbGetServerInfo :: Proto.RpbGetServerInfoReq -> EncodedRequest
encodeRpbGetServerInfo =
  encode 7

encodeRpbIndex :: Proto.RpbIndexReq -> EncodedRequest
encodeRpbIndex =
  encode 25

encodeRpbListBuckets :: Proto.RpbListBucketsReq -> EncodedRequest
encodeRpbListBuckets =
  encode 15

encodeRpbListKeys :: Proto.RpbListKeysReq -> EncodedRequest
encodeRpbListKeys =
  encode 17

encodeRpbMapRed :: Proto.RpbMapRedReq -> EncodedRequest
encodeRpbMapRed =
  encode 23

encodeRpbPing :: Proto.RpbPingReq -> EncodedRequest
encodeRpbPing =
  encode 1

encodeRpbPut :: Proto.RpbPutReq -> EncodedRequest
encodeRpbPut =
  encode 11

encodeRpbResetBucket :: Proto.RpbResetBucketReq -> EncodedRequest
encodeRpbResetBucket =
  encode 29

encodeRpbSearchQuery :: Proto.RpbSearchQueryReq -> EncodedRequest
encodeRpbSearchQuery =
  encode 27

encodeRpbSetBucket :: Proto.RpbSetBucketReq -> EncodedRequest
encodeRpbSetBucket =
  encode 21

encodeRpbSetBucketType :: Proto.RpbSetBucketTypeReq -> EncodedRequest
encodeRpbSetBucketType =
  encode 32

encodeRpbYokozunaIndexDelete :: Proto.RpbYokozunaIndexDeleteReq -> EncodedRequest
encodeRpbYokozunaIndexDelete =
  encode 57

encodeRpbYokozunaIndexGet :: Proto.RpbYokozunaIndexGetReq -> EncodedRequest
encodeRpbYokozunaIndexGet =
  encode 54

encodeRpbYokozunaIndexPut :: Proto.RpbYokozunaIndexPutReq -> EncodedRequest
encodeRpbYokozunaIndexPut =
  encode 56

encodeRpbYokozunaSchemaGet :: Proto.RpbYokozunaSchemaGetReq -> EncodedRequest
encodeRpbYokozunaSchemaGet =
  encode 58

encodeRpbYokozunaSchemaPut :: Proto.RpbYokozunaSchemaPutReq -> EncodedRequest
encodeRpbYokozunaSchemaPut =
  encode 60

encode :: Proto.Message a => Word8 -> a -> EncodedRequest
encode code (Proto.encodeMessage -> PS (ForeignPtr addr _) offset len) =
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
