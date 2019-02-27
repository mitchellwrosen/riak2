module Libriak.Response
  ( Response(..)
  , EncodedResponse(..)
  , decodeResponse
  , DecodeError(..)
  ) where

import Libriak.Proto

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bifunctor           (bimap)
import Data.ByteString          (ByteString)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.Ptr                  (Ptr(..))

import qualified Data.ByteString.Internal as ByteString
import qualified Data.ProtoLens           as Proto


data Response
  = RespDtFetch DtFetchResp
  | RespDtUpdate DtUpdateResp
  | RespRpbDel RpbDelResp
  | RespRpbError RpbErrorResp
  | RespRpbGet RpbGetResp
  | RespRpbGetBucket RpbGetBucketResp
  | RespRpbGetServerInfo RpbGetServerInfoResp
  | RespRpbIndex RpbIndexResp
  | RespRpbListBuckets RpbListBucketsResp
  | RespRpbListKeys RpbListKeysResp
  | RespRpbMapRed RpbMapRedResp
  | RespRpbPing RpbPingResp
  | RespRpbPut RpbPutResp
  | RespRpbResetBucket RpbResetBucketResp
  | RespRpbSearchQuery RpbSearchQueryResp
  | RespRpbSetBucket RpbSetBucketResp
  | RespRpbYokozunaIndexGet RpbYokozunaIndexGetResp
  | RespRpbYokozunaSchemaGet RpbYokozunaSchemaGetResp
  deriving stock (Show)

-- | An encoded response, which consists of a 1-byte message code and a protobuf
-- payload. The 4-byte big-endian length prefix has already been stripped.
newtype EncodedResponse
  = EncodedResponse { unEncodedResponse :: ByteArray }

decodeResponse :: EncodedResponse -> Either DecodeError Response
decodeResponse (EncodedResponse bytes) =
  decode (indexByteArray bytes 0) (runST makeByteStringFromPayload)
  where
    makeByteStringFromPayload :: ST s ByteString
    makeByteStringFromPayload =
      unsafeIOToST
        (ByteString.create
          payloadLen
          (\(Ptr addr) -> copyByteArrayToAddr (Addr addr) bytes 1 payloadLen))

    payloadLen :: Int
    payloadLen =
      sizeofByteArray bytes - 1

decode :: Word8 -> ByteString -> Either DecodeError Response
decode code bytes =
  case code of
    0  -> decode' RespRpbError
    2  -> Right (RespRpbPing Proto.defMessage)
    8  -> decode' RespRpbGetServerInfo
    10 -> decode' RespRpbGet
    12 -> decode' RespRpbPut
    14 -> Right (RespRpbDel Proto.defMessage)
    16 -> decode' RespRpbListBuckets
    18 -> decode' RespRpbListKeys
    20 -> decode' RespRpbGetBucket
    22 -> Right (RespRpbSetBucket Proto.defMessage)
    24 -> decode' RespRpbMapRed
    26 -> decode' RespRpbIndex
    28 -> decode' RespRpbSearchQuery
    30 -> Right (RespRpbResetBucket Proto.defMessage)
    55 -> decode' RespRpbYokozunaIndexGet
    59 -> decode' RespRpbYokozunaSchemaGet
    81 -> decode' RespDtFetch
    83 -> decode' RespDtUpdate
    _  -> Left (UnknownMessageCode code bytes)

  where
    decode' ::
         Proto.Message a
      => (a -> Response)
      -> Either DecodeError Response
    decode' f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)
