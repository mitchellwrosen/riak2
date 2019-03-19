-- | A thin wrapper around a socket. Sends and receives length-prefixed
-- payloads.
--
-- Like the underlying socket, this is not a thread-safe abstraction.

module Libriak.Connection
  ( Connection
  , withConnection
  , connect
  , disconnect
  , send
  , receive
  , ConnectionError(..)
  ) where

import Libriak.Request  (EncodedRequest(..))
import Libriak.Response (EncodedResponse(..))

import Control.Monad.Primitive  (RealWorld)
import Data.Bifunctor           (bimap, first)
import Data.Coerce              (coerce)
import Data.Kind                (Type)
import Data.Primitive.Addr      (Addr(..), copyAddrToByteArray)
import Data.Primitive.ByteArray
import Data.Word                (Word32, byteSwap32)
import GHC.ByteOrder            (ByteOrder(..), targetByteOrder)
import GHC.Conc                 (TVar)
import GHC.Ptr                  (Ptr(..))
import Socket.Stream.IPv4       (CloseException(..), ConnectException(..),
                                 Endpoint(..), Interruptibility(..),
                                 ReceiveException(..), SendException(..))

import qualified Data.ByteString.Unsafe as ByteString
import qualified Socket.Stream.IPv4     as Socket

gSendBufferSize :: Int
gSendBufferSize =
  4096

data Connection
  = Connection
  { connection :: Socket.Connection
    -- ^ Underlying connection.
  , sendBuffer :: MutableByteArray RealWorld
    -- ^ Fixed-size send buffer. Used during a send as a scrap buffer to fill up
    -- and send out. Not useful between sends.
  }

data ConnectionError :: Type where
  -- | The socket write channel is shut down.
  LocalShutdown :: ConnectionError
  -- | The remote peer reset the connection.
  RemoteReset :: ConnectionError
  -- | The remote peer's write channel is shut down.
  RemoteShutdown :: ConnectionError
  -- | We timed out waiting for a message from the remote peer.
  RemoteTimeout :: ConnectionError
  deriving stock (Eq, Show)

-- | Acquire a connection.
--
-- /Throws/. This function will never throw an exception.
withConnection ::
     TVar Bool
  -> Endpoint
  -> (Either CloseException () -> a -> IO b)
  -> (Connection -> IO a)
  -> IO (Either (ConnectException 'Interruptible) b)
withConnection timeoutVar endpoint onTeardown onSuccess =
  Socket.interruptibleWithConnection timeoutVar endpoint onTeardown $ \connection -> do
    sendBuffer :: MutableByteArray RealWorld <-
      newByteArray gSendBufferSize

    onSuccess Connection
      { connection = connection
      , sendBuffer = sendBuffer
      }

-- | Acquire a connection.
--
-- /Throws/. This function will never throw an exception.
connect ::
     TVar Bool
  -> Endpoint
  -> IO (Either (ConnectException 'Interruptible) Connection)
connect timeoutVar endpoint =
  Socket.interruptibleConnect timeoutVar endpoint >>= \case
    Left err ->
      pure (Left err)

    Right connection -> do
      sendBuffer :: MutableByteArray RealWorld <-
        newByteArray gSendBufferSize

      pure (Right Connection
        { connection = connection
        , sendBuffer = sendBuffer
        })

disconnect ::
     Connection
  -> IO (Either CloseException ())
disconnect Connection { connection } =
  Socket.disconnect connection

-- | Send a request.
--
-- /Throws/. This function will never throw an exception.
--
-- TODO writev
send ::
     Connection -- ^ Connection
  -> EncodedRequest -- ^ Request
  -> IO (Either ConnectionError ())
send Connection { connection, sendBuffer } (EncodedRequest code bytes) = do
  ByteString.unsafeUseAsCStringLen bytes $ \(Ptr addr, len) -> do
    lenBytes <- bigEndianWord32ByteArray (fromIntegral len + 1)
    copyByteArray sendBuffer 0 lenBytes 0 4
    copyByteArray sendBuffer 4 code 0 1

    -- Happy path: fits in one send call
    -- Unhappy path: send length + code, then send payload
    if len <= gSendBufferSize - 5
      then do
        copyAddrToByteArray sendBuffer 5 (Addr addr) len
        first fromSendException <$>
          Socket.sendMutableByteArraySlice connection sendBuffer 0 (len + 5)
      else
        Socket.sendMutableByteArraySlice connection sendBuffer 0 5 >>= \case
          Left err ->
            pure (Left (fromSendException err))

          Right () ->
            Socket.sendAddr connection (Addr addr) len >>= \case
              Left err ->
                pure (Left (fromSendException err))

              Right () ->
                pure (Right ())

  where
    bigEndianWord32ByteArray :: Word32 -> IO ByteArray
    bigEndianWord32ByteArray word = do
      bytes <- newByteArray 4
      writeByteArray bytes 0 (swap32 word)
      unsafeFreezeByteArray bytes

-- | Receive a length-prefixed byte array.
--
-- This function currently isn't very smart, it first receives the length, then
-- receives the packet. It would probably be better to use a buffer so we don't
-- call receive so frequently on the underlying socket.
receive ::
     TVar Bool
  -> Connection
  -> IO (Either ConnectionError EncodedResponse)
receive timeoutVar connection =
  bimap fromReceiveException coerce <$>
    receive_ timeoutVar connection

receive_ ::
     TVar Bool
  -> Connection
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
receive_ timeoutVar Connection { connection } =
  receiveBigEndianWord32 connection timeoutVar >>= \case
    Left err ->
      pure (Left err)

    Right len ->
      Socket.interruptibleReceiveByteArray
        timeoutVar
        connection
        (fromIntegral len)

receiveBigEndianWord32 ::
     Socket.Connection
  -> TVar Bool
  -> IO (Either (ReceiveException 'Interruptible) Word32)
receiveBigEndianWord32 connection timeoutVar =
  (fmap.fmap)
    parse
    (Socket.interruptibleReceiveByteArray timeoutVar connection 4)

  where
    parse :: ByteArray -> Word32
    parse bytes =
      swap32 (indexByteArray bytes 0)

fromSendException :: SendException 'Uninterruptible -> ConnectionError
fromSendException = \case
  SendReset    -> RemoteReset
  SendShutdown -> LocalShutdown

fromReceiveException :: ReceiveException 'Interruptible -> ConnectionError
fromReceiveException = \case
  ReceiveInterrupted -> RemoteTimeout
  ReceiveReset -> RemoteReset
  ReceiveShutdown -> RemoteShutdown

swap32 :: Word32 -> Word32
swap32 =
  case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap32
