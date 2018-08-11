{-# LANGUAGE DefaultSignatures, ScopedTypeVariables, TypeApplications #-}

module Riak.Internal.Request
  ( Request
  , requestToMessage

  , RpbPingReq(..)
  , RpbGetServerInfoReq(..)
  ) where

import Data.ByteString (ByteString)

import qualified Data.ProtoLens as Proto

import Proto.Riak
import Riak.Internal.Message

class Request a where
  requestCode :: MessageCode a

  requestEncode :: a -> ByteString
  default requestEncode :: Proto.Message a => a -> ByteString
  requestEncode = Proto.encodeMessage

instance Request RpbGetBucketReq   where requestCode = 19
instance Request RpbGetReq         where requestCode =  9
instance Request RpbListBucketsReq where requestCode = 15
instance Request RpbListKeysReq    where requestCode = 17
instance Request RpbResetBucketReq where requestCode = 29
instance Request RpbSetBucketReq   where requestCode = 21

requestToMessage :: forall a. Request a => a -> Message
requestToMessage req =
  Message (unMessageCode (requestCode @a)) (requestEncode req)

data RpbPingReq
  = RpbPingReq

instance Request RpbPingReq where
  requestCode = 1
  requestEncode _ = mempty

data RpbGetServerInfoReq
  = RpbGetServerInfoReq

instance Request RpbGetServerInfoReq where
  requestCode = 7
  requestEncode _ = mempty
