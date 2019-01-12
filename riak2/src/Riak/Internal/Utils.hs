module Riak.Internal.Utils where

import Data.Maybe (fromJust)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.ByteString.Lex.Integral as ByteString

import Riak.Internal.Panic
import Riak.Internal.Prelude

-- | Pack a non-negative 'Int64' as an ASCII byte array.
int2bs :: Int64 -> ByteString
int2bs n
  | n >= 0    = fromJust (ByteString.packDecimal n)
  | otherwise = Latin1.cons '-' (fromJust (ByteString.packDecimal (-n)))

-- | Unsafely unpack an ASCII byte array as an 'Int64'.
bs2int :: ByteString -> Int64
bs2int bytes =
  case ByteString.readSigned ByteString.readDecimal bytes of
    Just (n, leftovers) | ByteString.null leftovers ->
      n

    result ->
      impurePanic "bs2int"
        ( ("bytes",  bytes)
        , ("result", result)
        )
