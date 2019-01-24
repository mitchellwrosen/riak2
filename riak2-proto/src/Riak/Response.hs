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

instance Response ErrorResponse               where code =  0
instance Response GetBucketPropertiesResponse where code = 20
instance Response GetCrdtResponse             where code = 81
instance Response GetResponse                 where code = 10
instance Response GetServerInfoResponse       where code =  8
instance Response IndexResponse               where code = 26
instance Response MapReduceResponse           where code = 24
instance Response PutResponse                 where code = 12
instance Response StreamBucketsResponse       where code = 16
instance Response StreamKeysResponse          where code = 18
instance Response UpdateCrdtResponse          where code = 83

-- instance Response RpbSearchQueryResp       where code = 28
-- instance Response RpbYokozunaIndexGetResp  where code = 55
-- instance Response RpbYokozunaSchemaGetResp where code = 59

instance Response DeleteResponse where
  code = 14
  decode _ = Right DeleteResponse

instance Response EmptyPutResponse where
  code = 12
  decode _ = Right EmptyPutResponse

instance Response PingResponse where
  code = 2
  decode _ = Right PingResponse

instance Response ResetBucketPropertiesResponse where
  code = 30
  decode _ = Right ResetBucketPropertiesResponse

instance Response SetBucketPropertiesResponse where
  code = 22
  decode _ = Right SetBucketPropertiesResponse


data ParseError
  = UnexpectedMessageCode !Message !Word8
  | ProtobufDecodeError !Message !String
  deriving stock (Show)
  deriving anyclass (Exception)


parse ::
     forall a.
     Response a
  => Message
  -> Either ParseError (Either ErrorResponse a)
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
