module Riak.Key
  ( Key(..)
  ) where

import Riak.Internal.Prelude
import Riak.Bucket (Bucket)

import Data.Text.Encoding (decodeUtf8')

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.Text              as Text


-- | A bucket type, bucket, and key
data Key
  = Key !Bucket !ByteString
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- | For debugging; assumes buckets are UTF-8 encoded, but falls back to
-- base64-encoding.
instance Show Key where
  show :: Key -> String
  show (Key bucket key) =
    show bucket
    ++ " " ++
    either
      (const ("base64:" ++ Latin1.unpack (Base64.encode key)))
      Text.unpack
      (decodeUtf8' key)
