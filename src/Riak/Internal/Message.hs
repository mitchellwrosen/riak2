{-# LANGUAGE PatternSynonyms #-}

module Riak.Internal.Message where

import Data.ByteString (ByteString)
import Data.Word

-- | A 'Message' is a single message sent by both the server and client. On the
-- wire, it consists of a 4-byte big-endian length, 1-byte message code, and
-- encoded protobuf payload.
data Message
  = Message
      !Word8      -- Message code
      !ByteString -- Message payload

-- http://docs.basho.com/riak/kv/2.2.3/developing/api/protocol-buffers/#message-codes

pattern CodeRpbErrorResp :: Word8
pattern CodeRpbErrorResp = 0

pattern CodeRpbPingReq :: Word8
pattern CodeRpbPingReq = 1

pattern CodeRpbPingResp :: Word8
pattern CodeRpbPingResp = 2

-- TODO The rest of the message codes
