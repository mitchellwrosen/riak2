module Riak.Proto
  ( RpbDelResp(..)
  , RpbEmptyPutResp(..)
  , RpbGetServerInfoReq(..)
  , RpbPingReq(..)
  , RpbPingResp(..)
  , RpbResetBucketResp(..)
  , RpbSetBucketResp(..)
  , RpbSetBucketTypeResp(..)
  , module Proto.Riak
  ) where

import Proto.Riak

data RpbDelResp           = RpbDelResp           deriving stock (Show)
data RpbEmptyPutResp      = RpbEmptyPutResp      deriving stock (Show)
data RpbGetServerInfoReq  = RpbGetServerInfoReq  deriving stock (Show)
data RpbPingReq           = RpbPingReq           deriving stock (Show)
data RpbPingResp          = RpbPingResp          deriving stock (Show)
data RpbResetBucketResp   = RpbResetBucketResp   deriving stock (Show)
data RpbSetBucketResp     = RpbSetBucketResp     deriving stock (Show)
data RpbSetBucketTypeResp = RpbSetBucketTypeResp deriving stock (Show)
