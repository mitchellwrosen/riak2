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

instance Request DeleteRequest                  where code = 13
instance Request GetBucketPropertiesRequest     where code = 19
instance Request GetBucketTypePropertiesRequest where code = 31
instance Request GetCrdtRequest                 where code = 80
instance Request GetRequest                     where code =  9
instance Request IndexRequest                   where code = 25
instance Request MapReduceRequest               where code = 23
instance Request PingRequest                    where code =  1
instance Request PutRequest                     where code = 11
instance Request ResetBucketPropertiesRequest   where code = 29
instance Request SetBucketPropertiesRequest     where code = 21
instance Request SetBucketTypePropertiesRequest where code = 31
instance Request StreamBucketsRequest           where code = 15
instance Request StreamKeysRequest              where code = 17
instance Request UpdateCrdtRequest              where code = 82

-- instance Request RpbSearchQueryReq         where code = 27
-- instance Request RpbYokozunaIndexDeleteReq where code = 57
-- instance Request RpbYokozunaIndexGetReq    where code = 54
-- instance Request RpbYokozunaIndexPutReq    where code = 56
-- instance Request RpbYokozunaSchemaGetReq   where code = 58
-- instance Request RpbYokozunaSchemaPutReq   where code = 60

toMessage :: forall a. Request a => a -> Message
toMessage request =
  Message (coerce (Riak.Request.code @a)) (encode request)

instance Request GetServerInfoRequest where
  code = 7
  encode _ = mempty
