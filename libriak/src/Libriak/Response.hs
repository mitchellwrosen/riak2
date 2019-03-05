{-# LANGUAGE MagicHash #-}

module Libriak.Response
  ( Response(..)
  , EncodedResponse(..)
  , decodeResponse
  , responseDone
  , DecodeError(..)
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
  UnexpectedResponse :: Word8 -> Response code -> DecodeError
  UnexpectedMessageCode :: Word8 -> Word8 -> DecodeError
  deriving anyclass (Exception)

deriving instance Show DecodeError

data Response :: Nat -> Type where
  RespRpbError             :: Proto.RpbErrorResp             -> Response 0
  RespRpbPing              :: Proto.RpbPingResp              -> Response 2
  RespRpbGetServerInfo     :: Proto.RpbGetServerInfoResp     -> Response 8
  RespRpbGet               :: Proto.RpbGetResp               -> Response 10
  RespRpbPut               :: Proto.RpbPutResp               -> Response 12
  RespRpbDel               :: Proto.RpbDelResp               -> Response 14
  RespRpbListBuckets       :: Proto.RpbListBucketsResp       -> Response 16
  RespRpbListKeys          :: Proto.RpbListKeysResp          -> Response 18
  RespRpbGetBucket         :: Proto.RpbGetBucketResp         -> Response 20
  RespRpbSetBucket         :: Proto.RpbSetBucketResp         -> Response 22
  RespRpbMapRed            :: Proto.RpbMapRedResp            -> Response 24
  RespRpbIndex             :: Proto.RpbIndexResp             -> Response 26
  RespRpbSearchQuery       :: Proto.RpbSearchQueryResp       -> Response 28
  RespRpbResetBucket       :: Proto.RpbResetBucketResp       -> Response 30
  RespRpbYokozunaIndexGet  :: Proto.RpbYokozunaIndexGetResp  -> Response 55
  RespRpbYokozunaSchemaGet :: Proto.RpbYokozunaSchemaGetResp -> Response 59
  RespDtFetch              :: Proto.DtFetchResp              -> Response 81
  RespDtUpdate             :: Proto.DtUpdateResp             -> Response 83

deriving stock instance Show (Response code)

-- | An encoded response, which consists of a 1-byte message code and a protobuf
-- payload. The 4-byte big-endian length prefix has already been stripped.
newtype EncodedResponse
  = EncodedResponse { unEncodedResponse :: ByteArray }

decodeResponse ::
     KnownNat code
  => EncodedResponse
  -> Either DecodeError (Either (Response 0) (Response code))
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

decode ::
     forall code.
     KnownNat code
  => Word8
  -> ByteString
  -> Either DecodeError (Either (Response 0) (Response code))
decode actual bytes
  | actual == 0 =
      second (Left . RespRpbError) (decodeBytes bytes)
  | actual == expected =
      Right <$> decodeExpected actual bytes
  | otherwise =
      case decodeExpected actual bytes of
        -- Didn't even decode as the unexpected response, it's garbage data
        Left _ ->
          Left (UnexpectedMessageCode actual expected)

        -- We got someone else's response somehow.
        Right response ->
          Left (UnexpectedResponse expected response)

  where
    expected :: Word8
    expected =
      fromIntegral (natVal' (proxy# :: Proxy# code))

decodeExpected ::
     forall code.
     Word8
  -> ByteString
  -> Either DecodeError (Response code)
decodeExpected code bytes =
  case code of
    2  -> recode (Right (RespRpbPing Proto.defMessage))
    8  -> recode (RespRpbGetServerInfo <$> decodeBytes bytes)
    10 -> recode (RespRpbGet <$> decodeBytes bytes)
    12 -> recode (RespRpbPut <$> decodeBytes bytes)
    14 -> recode (Right (RespRpbDel Proto.defMessage))
    16 -> recode (RespRpbListBuckets <$> decodeBytes bytes)
    18 -> recode (RespRpbListKeys <$> decodeBytes bytes)
    20 -> recode (RespRpbGetBucket <$> decodeBytes bytes)
    22 -> recode (Right (RespRpbSetBucket Proto.defMessage))
    24 -> recode (RespRpbMapRed <$> decodeBytes bytes)
    26 -> recode (RespRpbIndex <$> decodeBytes bytes)
    28 -> recode (RespRpbSearchQuery <$> decodeBytes bytes)
    30 -> recode (Right (RespRpbResetBucket Proto.defMessage))
    55 -> recode (RespRpbYokozunaIndexGet <$> decodeBytes bytes)
    59 -> recode (RespRpbYokozunaSchemaGet <$> decodeBytes bytes)
    81 -> recode (RespDtFetch <$> decodeBytes bytes)
    83 -> recode (RespDtUpdate <$> decodeBytes bytes)
    _  -> error ("unknown message code " ++ show code)

decodeBytes ::
     forall a.
     Proto.Message a
  => ByteString
  -> Either DecodeError a
decodeBytes bytes =
  first (ProtobufDecodeError bytes) (Proto.decodeMessage bytes)

recode ::
     Either DecodeError (Response a)
  -> Either DecodeError (Response b)
recode =
  unsafeCoerce

responseDone :: Response code -> Bool
responseDone = \case
  RespRpbListBuckets response -> response ^. Proto.done
  RespRpbListKeys    response -> response ^. Proto.done
  RespRpbMapRed      response -> response ^. Proto.done
  RespRpbIndex       response -> response ^. Proto.done

  RespRpbError{}             -> True
  RespRpbPing{}              -> True
  RespRpbGetServerInfo{}     -> True
  RespRpbGet{}               -> True
  RespRpbPut{}               -> True
  RespRpbDel{}               -> True
  RespRpbGetBucket{}         -> True
  RespRpbSetBucket{}         -> True
  RespRpbSearchQuery{}       -> True
  RespRpbResetBucket{}       -> True
  RespRpbYokozunaIndexGet{}  -> True
  RespRpbYokozunaSchemaGet{} -> True
  RespDtFetch{}              -> True
  RespDtUpdate{}             -> True
