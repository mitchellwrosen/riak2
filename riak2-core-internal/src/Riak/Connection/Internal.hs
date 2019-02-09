-- | A thin wrapper around a socket. Sends and receives length-prefixed
-- payloads.
--
-- Like the underlying socket, this is not a thread-safe abstraction.

module Riak.Connection.Internal
  ( Connection
  , withConnection
  , send
  , receive
    -- * Re-exports
  , Endpoint(..)
  , SocketException(..)
  , Context(..)
  , Reason(..)
  ) where

import Control.Monad.Primitive  (RealWorld)
import Data.Bits                (unsafeShiftL, unsafeShiftR, (.|.))
import Data.Primitive.ByteArray
import Data.Word                (Word32, Word8)
import Socket.Stream.IPv4       (Context(..), Endpoint(..), Reason(..),
                                 SocketException(..))

import qualified Socket.Stream.IPv4 as Socket


gSendBufferSize :: Int
gSendBufferSize =
  4096

data Connection
  = Connection
  { connection :: !(Socket.Connection)
    -- ^ Underlying connection.
  , sendbuf :: !(MutableByteArray RealWorld)
    -- ^ Fixed-size send buffer. Used during a send as a scrap buffer to fill up
    -- and send out. Not useful between sends.
  }

-- | Acquire a connection.
--
-- /Throws/. This function will never throw an exception.
withConnection ::
     Endpoint
  -> (Connection -> IO a)
  -> IO (Either SocketException a)
withConnection endpoint callback = do
  Socket.withConnection endpoint $ \connection -> do
    sendbuf :: MutableByteArray RealWorld <-
      newByteArray gSendBufferSize

    callback Connection
      { connection = connection
      , sendbuf = sendbuf
      }

-- | Send a length-prefixed payload, which is composed of arbitrarily-sized byte
-- array fragments. Sends as many bytes as possible (up to 4kb) per syscall.
--
-- /Throws/. This function will never throw an exception.
send ::
     Connection -- ^ Connection
  -> [ByteArray] -- ^ Payload
  -> IO (Either SocketException ())
send conn payload = do
  lenBytes :: ByteArray <-
    bigEndianWord32ByteArray (fromIntegral payloadLen)

  sendall conn (lenBytes : payload)

  where
    payloadLen :: Int
    payloadLen =
      sum (map sizeofByteArray payload)

    bigEndianWord32ByteArray :: Word32 -> IO ByteArray
    bigEndianWord32ByteArray word = do
      mbytes <- newByteArray 4
      writeByteArray mbytes 0 (fromIntegral (unsafeShiftR word 24) :: Word8)
      writeByteArray mbytes 1 (fromIntegral (unsafeShiftR word 16) :: Word8)
      writeByteArray mbytes 2 (fromIntegral (unsafeShiftR word  8) :: Word8)
      writeByteArray mbytes 3 (fromIntegral (             word   ) :: Word8)
      unsafeFreezeByteArray mbytes

sendall :: Connection -> [ByteArray] -> IO (Either SocketException ())
sendall Connection { sendbuf, connection } =
  loop 0

  where
    loop :: Int -> [ByteArray] -> IO (Either SocketException ())
    loop !buffered = \case
      [] ->
        Socket.sendMutableByteArraySlice connection sendbuf 0 buffered

      bytes : bytess ->
        let
          nbytes :: Int
          nbytes =
            sizeofByteArray bytes
        in
          case compare (buffered + nbytes) gSendBufferSize of
            LT -> do
              copyByteArray sendbuf buffered bytes 0 nbytes
              loop (buffered + nbytes) bytess

            EQ -> do
              copyByteArray sendbuf buffered bytes 0 nbytes
              loop 0 bytess

            GT -> do
              -- Running example:
              --
              -- We have a 100 byte buffer, 98 bytes are filled with 'X'. We
              -- wish to send a 6 byte array 'ABCDEF'. So, we're going to buffer
              -- 'AB' send the full buffer, then loop with the unsent 'CDEF'.

              -- Running example:
              --
              -- bytesSent =
              --   100 - 98 (2)
              let
                bytesSent :: Int
                bytesSent =
                  gSendBufferSize - buffered

              -- Running example:
              --
              -- bytesUnsent =
              --   98 + 6 - 100 (4)
              let
                bytesUnsent :: Int
                bytesUnsent =
                  buffered + nbytes - gSendBufferSize

              -- Running example:
              --
              -- mbytes <-
              --   'A B C D E F'
              mbytes :: MutableByteArray RealWorld <-
                unsafeThawByteArray bytes

              -- Running example:
              --
              -- sendbuf <-
              --   'X  X  X ...  X  A  B'
              --    0  1  2     97 98 99
              copyMutableByteArray
                sendbuf buffered mbytes 0 bytesSent

              Socket.sendMutableByteArray connection sendbuf >>= \case
                Left err ->
                  pure (Left err)

                Right () -> do
                  -- Running example:
                  --
                  -- mbytes <-
                  --   'C D E F E F'
                  moveByteArray mbytes 0 mbytes bytesSent bytesUnsent

                  -- Running example:
                  --
                  -- munsent <-
                  --   'C D E F'
                  munsent :: MutableByteArray RealWorld <-
                    resizeMutableByteArray mbytes bytesUnsent

                  unsent :: ByteArray <-
                    unsafeFreezeByteArray munsent

                  loop 0 (unsent : bytess)

-- | Receive a length-prefixed byte array.
--
-- This function currently isn't very smart, it first receives the length, then
-- receives the packet. It would probably be better to use a buffer so we don't
-- call receive so frequently on the underlying socket.
--
-- TODO benchmark receive
--
-- /Throws/. This function will never throw an exception.
receive :: Connection -> IO (Either SocketException ByteArray)
receive Connection { connection } =
  receiveBigEndianWord32 connection >>= \case
    Left err ->
      pure (Left err)

    Right len ->
      Socket.receiveByteArray connection (fromIntegral len)

receiveBigEndianWord32 :: Socket.Connection -> IO (Either SocketException Word32)
receiveBigEndianWord32 connection =
  fmap parse <$> Socket.receiveByteArray connection 4

  where
    parse :: ByteArray -> Word32
    parse bytes =
      unsafeShiftL (fromIntegral w0 :: Word32) 24 .|.
      unsafeShiftL (fromIntegral w1 :: Word32) 16 .|.
      unsafeShiftL (fromIntegral w2 :: Word32)  8 .|.
                   (fromIntegral w3 :: Word32)

      where
        w0, w1, w2, w3 :: Word8
        w0 = indexByteArray bytes 0
        w1 = indexByteArray bytes 1
        w2 = indexByteArray bytes 2
        w3 = indexByteArray bytes 3

