{-# LANGUAGE MagicHash #-}

module Libriak.Response
  ( EncodedResponse(..)
  , DecodeError(..)
  , decodeDtFetch
  , decodeDtUpdate
  , decodeRpbDel
  , decodeRpbGet
  , decodeRpbGetBucket
  , decodeRpbGetServerInfo
  , decodeRpbIndex
  , decodeRpbListBuckets
  , decodeRpbListKeys
  , decodeRpbMapRed
  , decodeRpbPing
  , decodeRpbPut
  , decodeRpbResetBucket
  , decodeRpbSearchQuery
  , decodeRpbSetBucket
  , decodeRpbYokozunaIndexGet
  , decodeRpbYokozunaSchemaGet
  ) where

import Control.Exception        (Exception)
import Control.Lens             ((^.))
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bifunctor           (first, second)
import Data.ByteString          (ByteString)
import Data.Kind                (Type)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Word                (Word8)
import GHC.Exts                 (Proxy#, proxy#)
import GHC.Ptr                  (Ptr(..))
import GHC.TypeLits             (KnownNat, Nat, natVal')
import Unsafe.Coerce

import qualified Data.ByteString.Internal as ByteString
import qualified Data.Riak.Proto          as Proto


data DecodeError :: Type where
  ProtobufDecodeError :: ByteString -> String -> DecodeError
  UnexpectedResponse :: Word8 -> Word8 -> ByteString -> DecodeError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | An encoded response, which consists of a 1-byte message code and a protobuf
-- payload. The 4-byte big-endian length prefix has already been stripped.
newtype EncodedResponse
  = EncodedResponse { unEncodedResponse :: ByteArray }

decodeDtFetch ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.DtFetchResp)
decodeDtFetch =
  decode 81

decodeDtUpdate ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.DtUpdateResp)
decodeDtUpdate =
  decode 83

decodeRpbDel ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbDelResp)
decodeRpbDel =
  decode 14

decodeRpbGet ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbGetResp)
decodeRpbGet =
  decode 10

decodeRpbGetBucket ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbGetBucketResp)
decodeRpbGetBucket =
  decode 20

decodeRpbGetServerInfo ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbGetServerInfoResp)
decodeRpbGetServerInfo =
  decode 8

decodeRpbIndex ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbIndexResp)
decodeRpbIndex =
  decode 26

decodeRpbListBuckets ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbListBucketsResp)
decodeRpbListBuckets =
  decode 16

decodeRpbListKeys ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbListKeysResp)
decodeRpbListKeys =
  decode 18

decodeRpbMapRed ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbMapRedResp)
decodeRpbMapRed =
  decode 24

decodeRpbPing ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbPingResp)
decodeRpbPing =
  decode 2

decodeRpbPut ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbPutResp)
decodeRpbPut =
  decode 12

decodeRpbResetBucket ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbResetBucketResp)
decodeRpbResetBucket =
  decode 30

decodeRpbSearchQuery ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbSearchQueryResp)
decodeRpbSearchQuery =
  decode 28

decodeRpbSetBucket ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbSetBucketResp)
decodeRpbSetBucket =
  decode 22

decodeRpbYokozunaIndexGet ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbYokozunaIndexGetResp)
decodeRpbYokozunaIndexGet =
  decode 55

decodeRpbYokozunaSchemaGet ::
     EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp Proto.RpbYokozunaSchemaGetResp)
decodeRpbYokozunaSchemaGet =
  decode 59

decode ::
     Proto.Message a
  => Word8
  -> EncodedResponse
  -> Either DecodeError (Either Proto.RpbErrorResp a)
decode expected (EncodedResponse bytes) =
  decode_ actual expected (runST makeByteStringFromPayload)

  where
    actual :: Word8
    actual =
      indexByteArray bytes 0

    makeByteStringFromPayload :: ST s ByteString
    makeByteStringFromPayload =
      unsafeIOToST
        (ByteString.create
          payloadLen
          (\(Ptr addr) -> copyByteArrayToAddr (Addr addr) bytes 1 payloadLen))

    payloadLen :: Int
    payloadLen =
      sizeofByteArray bytes - 1

decode_ ::
     Proto.Message a
  => Word8
  -> Word8
  -> ByteString
  -> Either DecodeError (Either Proto.RpbErrorResp a)
decode_ actual expected bytes
  | actual == 0 =
      Left <$> decode__ bytes
  | actual == expected =
      Right <$> decode__ bytes
  | otherwise =
      Left (UnexpectedResponse expected actual bytes)

decode__ ::
     forall a.
     Proto.Message a
  => ByteString
  -> Either DecodeError a
decode__ bytes =
  first (ProtobufDecodeError bytes) (Proto.decodeMessage bytes)

-- responseDone :: Response code -> Bool
-- responseDone = \case
--   RespRpbListBuckets response -> response ^. Proto.done
--   RespRpbListKeys    response -> response ^. Proto.done
--   RespRpbMapRed      response -> response ^. Proto.done
--   RespRpbIndex       response -> response ^. Proto.done

--   RespRpbError{}             -> True
--   RespRpbPing{}              -> True
--   RespRpbGetServerInfo{}     -> True
--   RespRpbGet{}               -> True
--   RespRpbPut{}               -> True
--   RespRpbDel{}               -> True
--   RespRpbGetBucket{}         -> True
--   RespRpbSetBucket{}         -> True
--   RespRpbSearchQuery{}       -> True
--   RespRpbResetBucket{}       -> True
--   RespRpbYokozunaIndexGet{}  -> True
--   RespRpbYokozunaSchemaGet{} -> True
--   RespDtFetch{}              -> True
--   RespDtUpdate{}             -> True
