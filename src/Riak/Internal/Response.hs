{-# LANGUAGE DefaultSignatures, DerivingStrategies, GeneralizedNewtypeDeriving,
             OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Riak.Internal.Response
  ( Response
  , parseResponse

  , RpbPingResp(..)
  ) where

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ProtoLens as Proto

import Proto.Riak
import Riak.Internal.Message
import Riak.Internal.Panic

class Response a where
  responseCode :: Code a

  responseDecode :: ByteString -> Either String a
  default responseDecode :: Proto.Message a => ByteString -> Either String a
  responseDecode = Proto.decodeMessage

instance Response RpbErrorResp         where responseCode = 0
instance Response RpbGetServerInfoResp where responseCode = 8
instance Response RpbListBucketsResp   where responseCode = 16

newtype Code a
  = Code { unCode :: Word8 }
  deriving newtype Num

data RpbPingResp
  = RpbPingResp

instance Response RpbPingResp where
  responseCode = 2
  responseDecode _ = pure RpbPingResp

parseResponse :: forall a. Response a => Message -> IO (Either RpbErrorResp a)
parseResponse (Message actual bytes)
  | actual == expected =
      Right <$> decodeResponse actual bytes

  | actual == 0 =
      Left <$> decodeResponse actual bytes

  | otherwise =
      panic "Unexpected message code"
        ( ("actual", actual)
        , ("expected", expected)
        )
 where
  expected :: Word8
  expected =
    unCode (responseCode @a)

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
