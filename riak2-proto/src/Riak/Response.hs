module Riak.Response
  ( Response(..)
  , parse
  , ParseError(..)
  ) where

import Riak.Message (Code(..), Message(..))
import Riak.Proto

import Control.Exception (Exception)
import Data.ByteString   (ByteString)
import Data.Coerce       (coerce)
import Data.Word         (Word8)

import qualified Data.ProtoLens as Proto


class Show a => Response a where
  code :: Code a

  decode :: ByteString -> Either String a
  default decode :: Proto.Message a => ByteString -> Either String a
  decode = Proto.decodeMessage

instance Response DtFetchResp              where code = 81
instance Response DtUpdateResp             where code = 83
instance Response RpbErrorResp             where code =  0
instance Response RpbGetBucketResp         where code = 20
instance Response RpbGetResp               where code = 10
instance Response RpbGetServerInfoResp     where code =  8
instance Response RpbIndexResp             where code = 26
instance Response RpbListBucketsResp       where code = 16
instance Response RpbListKeysResp          where code = 18
instance Response RpbMapRedResp            where code = 24
instance Response RpbPutResp               where code = 12
instance Response RpbSearchQueryResp       where code = 28
instance Response RpbYokozunaIndexGetResp  where code = 55
instance Response RpbYokozunaSchemaGetResp where code = 59

instance Response RpbDelResp where
  code = 14
  decode _ = Right RpbDelResp

instance Response RpbEmptyPutResp where
  code = 12
  decode _ = Right RpbEmptyPutResp

instance Response RpbPingResp where
  code = 2
  decode _ = Right RpbPingResp

instance Response RpbResetBucketResp where
  code = 30
  decode _ = Right RpbResetBucketResp

instance Response RpbSetBucketResp where
  code = 22
  decode _ = Right RpbSetBucketResp

instance Response RpbSetBucketTypeResp where
  code = 32
  decode _ = Right RpbSetBucketTypeResp


data ParseError
  = UnexpectedMessageCode !Message !Word8
  | ProtobufDecodeError !Message !String
  deriving stock (Show)
  deriving anyclass (Exception)


parse ::
     forall a.
     Response a
  => Message
  -> Either ParseError (Either RpbErrorResp a)
parse message@(Message actual bytes)
  | actual == expected =
      case decode bytes of
        Left err ->
          Left (ProtobufDecodeError message err)

        Right response ->
          Right (Right response)

  | actual == 0 =
      case decode bytes of
        Left err ->
          Left (ProtobufDecodeError message err)

        Right response ->
          Right (Left response)

  | otherwise =
      Left (UnexpectedMessageCode message expected)

  where
    expected :: Word8
    expected =
      coerce (Riak.Response.code @a)
