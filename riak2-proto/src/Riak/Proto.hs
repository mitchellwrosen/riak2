module Riak.Proto
  ( DeleteResponse(..)
  , EmptyPutResponse(..)
  , GetServerInfoRequest(..)
  , PingResponse(..)
  , ResetBucketPropertiesResponse(..)
  , SetBucketPropertiesResponse(..)
  , module Proto.Riak
  ) where

import Proto.Riak

data DeleteResponse                  = DeleteResponse                deriving stock (Show)
data EmptyPutResponse                = EmptyPutResponse              deriving stock (Show)
data GetServerInfoRequest            = GetServerInfoRequest          deriving stock (Show)
data PingResponse                    = PingResponse                  deriving stock (Show)
data ResetBucketPropertiesResponse   = ResetBucketPropertiesResponse deriving stock (Show)
data SetBucketPropertiesResponse     = SetBucketPropertiesResponse   deriving stock (Show)
data SetBucketTypePropertiesResponse = SetBucketTypePropertiesResponse   deriving stock (Show)
