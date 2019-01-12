module Riak.Internal.Response
  ( Response(..)
  , parseResponse
  , RpbDelResp(..)
  , RpbPingResp(..)
  , RpbResetBucketResp(..)
  , RpbSetBucketResp(..)
  , RpbSetBucketTypeResp(..)
  , RpbEmptyPutResp(..)
  ) where

import Data.Text.Encoding (decodeUtf8)

import qualified Data.ProtoLens as Proto

import Proto.Riak
import Riak.Internal.Message
import Riak.Internal.Panic
import Riak.Internal.Prelude
import Riak.Internal.Types

class Show a => Response a where
  responseCode :: MessageCode a

  responseDecode :: ByteString -> Either String a
  default responseDecode :: Proto.Message a => ByteString -> Either String a
  responseDecode = Proto.decodeMessage

instance Response DtFetchResp              where responseCode = 81
instance Response DtUpdateResp             where responseCode = 83
instance Response RpbErrorResp             where responseCode =  0
instance Response RpbGetBucketResp         where responseCode = 20
instance Response RpbGetResp               where responseCode = 10
instance Response RpbGetServerInfoResp     where responseCode =  8
instance Response RpbIndexResp             where responseCode = 26
instance Response RpbListBucketsResp       where responseCode = 16
instance Response RpbListKeysResp          where responseCode = 18
instance Response RpbMapRedResp            where responseCode = 24
instance Response RpbPutResp               where responseCode = 12
instance Response RpbSearchQueryResp       where responseCode = 28
instance Response RpbYokozunaIndexGetResp  where responseCode = 55
instance Response RpbYokozunaSchemaGetResp where responseCode = 59

data RpbDelResp
  = RpbDelResp
  deriving Show

instance Response RpbDelResp where
  responseCode = 14
  responseDecode _ = pure RpbDelResp

data RpbEmptyPutResp
  = RpbEmptyPutResp
  deriving Show

instance Response RpbEmptyPutResp where
  responseCode = 12
  responseDecode _ = pure RpbEmptyPutResp

data RpbPingResp
  = RpbPingResp
  deriving Show

instance Response RpbPingResp where
  responseCode = 2
  responseDecode _ = pure RpbPingResp

data RpbResetBucketResp
  = RpbResetBucketResp
  deriving Show

instance Response RpbResetBucketResp where
  responseCode = 30
  responseDecode _ = pure RpbResetBucketResp

data RpbSetBucketResp
  = RpbSetBucketResp
  deriving Show

instance Response RpbSetBucketResp where
  responseCode = 22
  responseDecode _ = pure RpbSetBucketResp

data RpbSetBucketTypeResp
  = RpbSetBucketTypeResp
  deriving Show

instance Response RpbSetBucketTypeResp where
  responseCode = 32
  responseDecode _ = pure RpbSetBucketTypeResp

parseResponse :: forall a. Response a => Message -> IO (Either RiakError (IO a))
parseResponse (Message actual bytes)
  | actual == expected =
      pure (Right (decodeResponse actual bytes))

  | actual == 0 =
      Left . toRiakError <$> decodeResponse actual bytes

  | otherwise =
      panic "Unexpected message code"
        ( ("actual", actual)
        , ("expected", expected)
        )
 where
  expected :: Word8
  expected =
    unMessageCode (responseCode @a)

  -- Code as of 2.2.3 is currently always 0, so just just toss it
  toRiakError :: RpbErrorResp -> RiakError
  toRiakError resp =
    RiakError (decodeUtf8 (resp ^. #errmsg))

decodeResponse :: Response a => Word8 -> ByteString -> IO a
decodeResponse code bytes =
  case responseDecode bytes of
    Left err ->
      panic "Protobuf decoding failure"
        ( ("code", code)
        , ("error", err)
        )

    Right x ->
      pure x
