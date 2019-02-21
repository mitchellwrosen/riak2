module RiakUtils where

import RiakPanic

import Control.Concurrent (threadDelay)
import Data.Maybe         (fromJust)

import qualified Data.ByteString              as ByteString
import qualified Data.ByteString.Char8        as Latin1
import qualified Data.ByteString.Lex.Integral as ByteString


-- | Keep retrying an action until it returns Nothing. Delay by the given number
-- of microseconds before the first retry, and exponentially back off.
retrying :: Int -> IO (Maybe a) -> IO a
retrying delay0 action =
  loop delay0
  where
    loop delay =
      action >>= \case
        Nothing -> do
          threadDelay delay
          loop (delay * 3 `div` 2)
        Just result ->
          pure result

-- | Pack an 'Int64' as an ASCII byte array.
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
