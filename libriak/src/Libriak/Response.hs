{-# LANGUAGE MagicHash #-}

module Libriak.Response
  ( Response(..)
  , EncodedResponse(..)
  , decodeResponse
  , responseDone
  , DecodeError(..)
  ) where

import Libriak.Proto hiding (DecodeError(..))

import qualified Libriak.Proto as Proto

import Control.Exception        (Exception)
import Control.Lens             ((^.))
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bifunctor           (bimap)
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


data DecodeError
  = ProtobufDecodeError !ByteString !String
  | UnexpectedMessageCode !Word8
  deriving stock (Show)
  deriving anyclass (Exception)

data Response :: Nat -> Type where
  RespRpbError             :: RpbErrorResp             -> Response 0
  RespRpbPing              :: RpbPingResp              -> Response 2
  RespRpbGetServerInfo     :: RpbGetServerInfoResp     -> Response 8
  RespRpbGet               :: RpbGetResp               -> Response 10
  RespRpbPut               :: RpbPutResp               -> Response 12
  RespRpbDel               :: RpbDelResp               -> Response 14
  RespRpbListBuckets       :: RpbListBucketsResp       -> Response 16
  RespRpbListKeys          :: RpbListKeysResp          -> Response 18
  RespRpbGetBucket         :: RpbGetBucketResp         -> Response 20
  RespRpbSetBucket         :: RpbSetBucketResp         -> Response 22
  RespRpbMapRed            :: RpbMapRedResp            -> Response 24
  RespRpbIndex             :: RpbIndexResp             -> Response 26
  RespRpbSearchQuery       :: RpbSearchQueryResp       -> Response 28
  RespRpbResetBucket       :: RpbResetBucketResp       -> Response 30
  RespRpbYokozunaIndexGet  :: RpbYokozunaIndexGetResp  -> Response 55
  RespRpbYokozunaSchemaGet :: RpbYokozunaSchemaGetResp -> Response 59
  RespDtFetch              :: DtFetchResp              -> Response 81
  RespDtUpdate             :: DtUpdateResp             -> Response 83

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
      doDecode (Left . RespRpbError)
  | actual == expected =
      case actual of
        2  -> recode (Right (Right (RespRpbPing Proto.defMessage)))
        8  -> recode (doDecode (Right . RespRpbGetServerInfo))
        10 -> recode (doDecode (Right . RespRpbGet))
        12 -> recode (doDecode (Right . RespRpbPut))
        14 -> recode (Right (Right (RespRpbDel Proto.defMessage)))
        16 -> recode (doDecode (Right . RespRpbListBuckets))
        18 -> recode (doDecode (Right . RespRpbListKeys))
        20 -> recode (doDecode (Right . RespRpbGetBucket))
        22 -> recode (Right (Right (RespRpbSetBucket Proto.defMessage)))
        24 -> recode (doDecode (Right . RespRpbMapRed))
        26 -> recode (doDecode (Right . RespRpbIndex))
        28 -> recode (doDecode (Right . RespRpbSearchQuery))
        30 -> recode (Right (Right (RespRpbResetBucket Proto.defMessage)))
        55 -> recode (doDecode (Right . RespRpbYokozunaIndexGet))
        59 -> recode (doDecode (Right . RespRpbYokozunaSchemaGet))
        81 -> recode (doDecode (Right . RespDtFetch))
        83 -> recode (doDecode (Right . RespDtUpdate))
        _  -> error ("unknown message code " ++ show actual)
  | otherwise =
      Left (UnexpectedMessageCode actual)


  where
    expected :: Word8
    expected =
      fromIntegral (natVal' (proxy# :: Proxy# code))

    doDecode ::
         forall a b.
         Proto.Message a
      => (a -> b)
      -> Either DecodeError b
    doDecode f =
      bimap (ProtobufDecodeError bytes) f (Proto.decodeMessage bytes)

recode ::
     Either DecodeError (Either (Response 0) (Response a))
  -> Either DecodeError (Either (Response 0) (Response b))
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
