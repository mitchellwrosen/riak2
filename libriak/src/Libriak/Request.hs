module Libriak.Request
  ( Request(..)
  , EncodedRequest(..)
  , encodeRequest
  ) where

import Control.Monad.ST
import Data.ByteString.Internal (ByteString(..))
import Data.Kind                (Type)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.ForeignPtr           (ForeignPtr(..))
import GHC.TypeLits             (Nat)

import qualified Data.ProtoLens  as Proto
import qualified Data.Riak.Proto as Proto


-- TODO request 33 is of riak_kv so handle unknown message code by retrying

-- | Request indexed by message code of the expected response.
data Request :: Nat -> Type where
  ReqDtFetch                :: Proto.DtFetchReq                -> Request 81
  ReqDtUpdate               :: Proto.DtUpdateReq               -> Request 83
  ReqRpbDel                 :: Proto.RpbDelReq                 -> Request 14
  ReqRpbGet                 :: Proto.RpbGetReq                 -> Request 10
  ReqRpbGetBucket           :: Proto.RpbGetBucketReq           -> Request 20
  ReqRpbGetBucketType       :: Proto.RpbGetBucketTypeReq       -> Request 20
  ReqRpbGetServerInfo       :: Proto.RpbGetServerInfoReq       -> Request 8
  ReqRpbIndex               :: Proto.RpbIndexReq               -> Request 26
  ReqRpbListBuckets         :: Proto.RpbListBucketsReq         -> Request 16
  ReqRpbListKeys            :: Proto.RpbListKeysReq            -> Request 18
  ReqRpbMapRed              :: Proto.RpbMapRedReq              -> Request 24
  ReqRpbPing                :: Proto.RpbPingReq                -> Request 2
  ReqRpbPut                 :: Proto.RpbPutReq                 -> Request 12
  ReqRpbResetBucket         :: Proto.RpbResetBucketReq         -> Request 30
  ReqRpbSearchQuery         :: Proto.RpbSearchQueryReq         -> Request 28
  ReqRpbSetBucket           :: Proto.RpbSetBucketReq           -> Request 22
  ReqRpbSetBucketType       :: Proto.RpbSetBucketTypeReq       -> Request 22
  ReqRpbYokozunaIndexDelete :: Proto.RpbYokozunaIndexDeleteReq -> Request 14
  ReqRpbYokozunaIndexGet    :: Proto.RpbYokozunaIndexGetReq    -> Request 55
  ReqRpbYokozunaIndexPut    :: Proto.RpbYokozunaIndexPutReq    -> Request 12
  ReqRpbYokozunaSchemaGet   :: Proto.RpbYokozunaSchemaGetReq   -> Request 59
  ReqRpbYokozunaSchemaPut   :: Proto.RpbYokozunaSchemaPutReq   -> Request 12

deriving stock instance Show (Request code)

newtype EncodedRequest
  = EncodedRequest { unEncodedRequest :: [ByteArray] }

encodeRequest :: Request code -> EncodedRequest
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
