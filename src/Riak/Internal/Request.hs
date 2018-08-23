{-# LANGUAGE DefaultSignatures, NoImplicitPrelude, ScopedTypeVariables,
             TypeApplications #-}

module Riak.Internal.Request
  ( Request
  , requestToMessage

  , RpbPingReq(..)
  , RpbGetServerInfoReq(..)
  ) where

import qualified Data.ProtoLens as Proto

import Proto.Riak
import Riak.Internal.Message
import Riak.Internal.Prelude

class Show a => Request a where
  requestCode :: MessageCode a

  requestEncode :: a -> ByteString
  default requestEncode :: Proto.Message a => a -> ByteString
  requestEncode = Proto.encodeMessage

instance Request DtFetchReq                where requestCode = 80
instance Request DtUpdateReq               where requestCode = 82
instance Request RpbDelReq                 where requestCode = 13
instance Request RpbGetBucketReq           where requestCode = 19
instance Request RpbGetBucketTypeReq       where requestCode = 31
instance Request RpbGetReq                 where requestCode =  9
instance Request RpbListBucketsReq         where requestCode = 15
instance Request RpbListKeysReq            where requestCode = 17
instance Request RpbMapRedReq              where requestCode = 23
instance Request RpbPutReq                 where requestCode = 11
instance Request RpbResetBucketReq         where requestCode = 29
instance Request RpbSetBucketReq           where requestCode = 21
instance Request RpbSetBucketTypeReq       where requestCode = 31
instance Request RpbYokozunaIndexDeleteReq where requestCode = 57
instance Request RpbYokozunaIndexGetReq    where requestCode = 54
instance Request RpbYokozunaIndexPutReq    where requestCode = 56
instance Request RpbYokozunaSchemaGetReq   where requestCode = 58
instance Request RpbYokozunaSchemaPutReq   where requestCode = 60

requestToMessage :: forall a. Request a => a -> Message
requestToMessage req =
  Message (unMessageCode (requestCode @a)) (requestEncode req)

data RpbPingReq
  = RpbPingReq
  deriving Show

instance Request RpbPingReq where
  requestCode = 1
  requestEncode _ = mempty

data RpbGetServerInfoReq
  = RpbGetServerInfoReq
  deriving Show

instance Request RpbGetServerInfoReq where
  requestCode = 7
  requestEncode _ = mempty
