module Riak.Response
  ( Response(..)
  , parse
  ) where

-- import Data.Text.Encoding (decodeUtf8)

import Riak.Message (Code(..), Message(..))
import Riak.Proto

import Control.Exception      (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString        (ByteString)
import Data.Coerce            (coerce)
import Data.Word              (Word8)

import qualified Data.ProtoLens as Proto


class Show a => Response a where
  code :: Code a

  decode :: MonadIO m => ByteString -> m a
  default decode :: (Proto.Message a, MonadIO m) => ByteString -> m a
  decode bytes =
    case Proto.decodeMessage bytes of
      Left message ->
        liftIO $ throwIO ProtobufDecodeFailure
          { message = message
          , bytes = bytes
          }

      Right value ->
        pure value

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
  decode _ = pure RpbDelResp

instance Response RpbEmptyPutResp where
  code = 12
  decode _ = pure RpbEmptyPutResp

instance Response RpbPingResp where
  code = 2
  decode _ = pure RpbPingResp

instance Response RpbResetBucketResp where
  code = 30
  decode _ = pure RpbResetBucketResp

instance Response RpbSetBucketResp where
  code = 22
  decode _ = pure RpbSetBucketResp

instance Response RpbSetBucketTypeResp where
  code = 32
  decode _ = pure RpbSetBucketTypeResp


data UnexpectedMessageCode
  = UnexpectedMessageCode
  { actual   :: !Message
  , expected :: !Word8
  } deriving stock (Show)
    deriving anyclass (Exception)

data ProtobufDecodeFailure
  = ProtobufDecodeFailure
  { message :: !String
  , bytes   :: !ByteString
  } deriving stock (Show)
    deriving anyclass (Exception)

parse ::
     forall a m.
     ( MonadIO m
     , Response a
     )
  => Message
  -> m (Either RpbErrorResp (m a))
parse message@(Message actual bytes)
  | actual == expected =
      pure (Right (decode bytes))

  | actual == 0 =
      Left <$> decode bytes

  | otherwise =
      liftIO $ throwIO UnexpectedMessageCode
        { actual = message
        , expected = expected
        }

  where
    expected :: Word8
    expected =
      coerce (Riak.Response.code @a)

    -- -- Code as of 2.2.3 is currently always 0, so just just toss it
    -- toRiakError :: RpbErrorResp -> RiakError
    -- toRiakError resp =
    --   RiakError (decodeUtf8 (resp ^. #errmsg))
