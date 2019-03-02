module RiakRequest
  ( Request(..)
  , EncodedRequest(..)
  , encodeRequest
  ) where

import Libriak.Proto
import Libriak.Request (EncodedRequest(..))

import Control.Monad.ST
import Data.ByteString.Internal (ByteString(..))
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import GHC.ForeignPtr           (ForeignPtr(..))
import GHC.TypeLits             (Nat)

import qualified Data.ProtoLens as Proto


-- TODO request 33 is of riak_kv so handle unknown message code by retrying

data Request :: Nat -> Type where
  ReqDtFetch                :: DtFetchReq                -> Request 80
  ReqDtUpdate               :: DtUpdateReq               -> Request 82
  ReqRpbDel                 :: RpbDelReq                 -> Request 13
  ReqRpbGet                 :: RpbGetReq                 -> Request 9
  ReqRpbGetBucket           :: RpbGetBucketReq           -> Request 19
  ReqRpbGetBucketType       :: RpbGetBucketTypeReq       -> Request 31
  ReqRpbGetServerInfo       :: RpbGetServerInfoReq       -> Request 7
  ReqRpbIndex               :: RpbIndexReq               -> Request 25
  ReqRpbListBuckets         :: RpbListBucketsReq         -> Request 15
  ReqRpbListKeys            :: RpbListKeysReq            -> Request 17
  ReqRpbMapRed              :: RpbMapRedReq              -> Request 23
  ReqRpbPing                :: RpbPingReq                -> Request 1
  ReqRpbPut                 :: RpbPutReq                 -> Request 11
  ReqRpbResetBucket         :: RpbResetBucketReq         -> Request 29
  ReqRpbSearchQuery         :: RpbSearchQueryReq         -> Request 27
  ReqRpbSetBucket           :: RpbSetBucketReq           -> Request 21
  ReqRpbSetBucketType       :: RpbSetBucketTypeReq       -> Request 32
  ReqRpbYokozunaIndexDelete :: RpbYokozunaIndexDeleteReq -> Request 57
  ReqRpbYokozunaIndexGet    :: RpbYokozunaIndexGetReq    -> Request 54
  ReqRpbYokozunaIndexPut    :: RpbYokozunaIndexPutReq    -> Request 56
  ReqRpbYokozunaSchemaGet   :: RpbYokozunaSchemaGetReq   -> Request 58
  ReqRpbYokozunaSchemaPut   :: RpbYokozunaSchemaPutReq   -> Request 60

deriving stock instance Show (Request code)

-- newtype EncodedRequest
--   = EncodedRequest { unEncodedRequest :: [ByteArray] }

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
