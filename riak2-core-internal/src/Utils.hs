module Utils
  ( wire
  ) where

import Data.Bits          (unsafeShiftR)
import Data.ByteString    (ByteString)
import Data.Word          (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (poke)

import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ProtoLens           as Proto

wire :: Proto.Message a => Word8 -> a -> ByteString
wire code (Proto.encodeMessage -> request) =
  ByteString.unsafeCreate (5 + ByteString.length request) $ \ptr -> do
    pokeWord32BE ptr (fromIntegral (1 + ByteString.length request))
    poke (ptr `plusPtr` 4) code
    pokeByteString (ptr `plusPtr` 5) request

pokeByteString :: Ptr Word8 -> ByteString -> IO ()
pokeByteString dst (ByteString.PS srcf offset len) =
  withForeignPtr srcf $ \src ->
    ByteString.memcpy dst (src `plusPtr` offset) len

pokeWord32BE :: Ptr Word8 -> Word32 -> IO ()
pokeWord32BE ptr word = do
  poke  ptr              (fromIntegral (unsafeShiftR word 24) :: Word8)
  poke (ptr `plusPtr` 1) (fromIntegral (unsafeShiftR word 16) :: Word8)
  poke (ptr `plusPtr` 2) (fromIntegral (unsafeShiftR word  8) :: Word8)
  poke (ptr `plusPtr` 3) (fromIntegral (             word   ) :: Word8)
