module Riak.Request
  ( Request(..)
  , toMessage
  ) where

import Riak.Message (Message(..), Code(..))
import Riak.Proto

import Data.ByteString (ByteString)
import Data.Coerce (coerce)

import qualified Data.ProtoLens as Proto


class Show a => Request a where
  code :: Code a

  encode :: a -> ByteString
  default encode :: Proto.Message a => a -> ByteString
  encode = Proto.encodeMessage

instance Request DtFetchReq                where code = 80
instance Request DtUpdateReq               where code = 82
instance Request RpbDelReq                 where code = 13
instance Request RpbGetBucketReq           where code = 19
instance Request RpbGetBucketTypeReq       where code = 31
instance Request RpbGetReq                 where code =  9
instance Request RpbIndexReq               where code = 25
instance Request RpbListBucketsReq         where code = 15
instance Request RpbListKeysReq            where code = 17
instance Request RpbMapRedReq              where code = 23
instance Request RpbPutReq                 where code = 11
instance Request RpbResetBucketReq         where code = 29
instance Request RpbSearchQueryReq         where code = 27
instance Request RpbSetBucketReq           where code = 21
instance Request RpbSetBucketTypeReq       where code = 31
instance Request RpbYokozunaIndexDeleteReq where code = 57
instance Request RpbYokozunaIndexGetReq    where code = 54
instance Request RpbYokozunaIndexPutReq    where code = 56
instance Request RpbYokozunaSchemaGetReq   where code = 58
instance Request RpbYokozunaSchemaPutReq   where code = 60

toMessage :: forall a. Request a => a -> Message
toMessage request =
  Message (coerce (Riak.Request.code @a)) (encode request)

instance Request RpbPingReq where
  code = 1
  encode _ = mempty

instance Request RpbGetServerInfoReq where
  code = 7
  encode _ = mempty
