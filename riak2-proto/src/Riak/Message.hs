module Riak.Message
  ( Message(..)
  , encode
  , parser
  , Code(..)
  ) where

import Data.Bits       (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Int        (Int32)
import Data.Word       (Word8)

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as Lazy (ByteString)

-- | A 'Message' is a single message sent by both the server and client. On the
-- wire, it consists of a 4-byte big-endian length, 1-byte message code, and
-- encoded protobuf payload.
data Message
  = Message
  { code :: !Word8
  , payload :: !ByteString
  } deriving stock (Show)

newtype Code a
  = Code { unCode :: Word8 }
  deriving newtype (Num)

encode :: Message -> Lazy.ByteString
encode (Message code bytes) =
  Builder.toLazyByteString
    (Builder.int32BE (fromIntegral (ByteString.length bytes + 1))
      <> Builder.word8 code
      <> Builder.byteString bytes)

parser :: Atto.Parser Message
parser = do
  len   <- int32be
  code  <- Atto.anyWord8
  if len > 1
    then Message code <$> Atto.take (fromIntegral (len-1))
    else pure (Message code mempty)
 where
  -- | Attoparsec parser for a 32-bit big-endian integer.
  int32be :: Atto.Parser Int32
  int32be = do
    w0 <- Atto.anyWord8
    w1 <- Atto.anyWord8
    w2 <- Atto.anyWord8
    w3 <- Atto.anyWord8
    pure $
      shiftL (fromIntegral w0) 24 .|.
      shiftL (fromIntegral w1) 16 .|.
      shiftL (fromIntegral w2)  8 .|.
              fromIntegral w3
