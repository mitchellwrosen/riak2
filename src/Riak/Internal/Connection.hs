{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Riak.Internal.Connection
  ( Connection
  , withConnection
  , send
  , recv
  , exchange1
  ) where

import Control.Monad
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Data.Bits               (shiftL, (.|.))
import Data.ByteString         (ByteString)
import Data.Int
import Data.IORef
import Data.Void
import Network.Socket          (AddrInfo(..), HostName, PortNumber, Socket,
                                SocketType(Stream), defaultHints, getAddrInfo)
import UnliftIO.Exception      (bracket)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Builder        as Builder
import qualified Data.ByteString.Lazy           as Lazy (ByteString)
import qualified Data.ByteString.Streaming      as Q
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)

import Proto.Riak
import Riak.Internal.Message
import Riak.Internal.Panic
import Riak.Internal.Response (Response, parseResponse)

-- | A non-thread-safe connection to Riak.
data Connection
  = Connection
      !Socket                         -- Server socket
      !(IORef (Q.ByteString IO Void)) -- Infinite input from server
      (Lazy.ByteString -> IO ())      -- Send bytes to server
-- TODO Make Connection thread-safe

withConnection
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Connection -> m a)
  -> m a
withConnection host port =
  bracket (connect host port) disconnect

connect :: MonadIO m => HostName -> PortNumber -> m Connection
connect host port = liftIO $ do
  info : _ <-
    let
      hints =
        defaultHints { addrSocketType = Stream }
    in
      getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Socket.socket (addrFamily info) (addrSocketType info) (addrProtocol info)

  Socket.connect socket (addrAddress info)

  let
    source :: Q.ByteString IO Void
    source =
      forever $ do
        bytes :: ByteString <-
          liftIO (Socket.recv socket 4096)
        if ByteString.null bytes
          -- TODO Properly handle Riak closing connection
          then error "Riak closed the connection"
          else Q.chunk bytes

  sourceRef :: IORef (Q.ByteString IO Void) <-
    newIORef source

  let
    sink :: Lazy.ByteString -> IO ()
    sink =
      Socket.sendAll socket

  pure (Connection socket sourceRef sink)

disconnect :: MonadIO m => Connection -> m ()
disconnect (Connection socket _ _) =
  liftIO (Socket.close socket)

-- | Send a 'Message' on a 'Connection'.
send :: Connection -> Message -> IO ()
send (Connection _ _ sink) (Message code bytes) =
  sink payload
 where
  payload :: Lazy.ByteString
  payload =
    Builder.toLazyByteString
      (Builder.int32BE (fromIntegral (ByteString.length bytes + 1))
        <> Builder.word8 code
        <> Builder.byteString bytes)

-- | Receive a 'Message' on a 'Connection'.
recv :: Connection -> IO Message
recv (Connection _ sourceRef _) = do
  source0 :: Q.ByteString IO Void <-
    readIORef sourceRef

  (len, source1) <-
    parsePanic int32be source0

  (code, source2) <-
    parsePanic Atto.anyWord8 source1

  if len > 1
    then do
      (bytes, source3) <-
        parsePanic (Atto.take (fromIntegral (len-1))) source2

      writeIORef sourceRef source3

      pure (Message code bytes)

    else do
      writeIORef sourceRef source2

      pure (Message code mempty)

exchange1 :: Response a => Connection -> Message -> IO (Either RpbErrorResp a)
exchange1 conn req = do
  send conn req
  recv conn >>= parseResponse

parsePanic
  :: Atto.Parser a
  -> Q.ByteString IO Void
  -> IO (a, Q.ByteString IO Void)
parsePanic parser bytes =
  parse parser bytes >>= \case
    EndOfInput v ->
      absurd v

    FailedParse _unconsumed context reason ->
      panic "Riak parse failure"
        ( ("context", context)
        , ("reason", reason)
        )

    SuccessfulParse x bytes' ->
      pure (x, bytes')

-- | Throwaway 'parse' result type.
data ParseResult a r
  = EndOfInput r
  | FailedParse !ByteString  ![String] !String
  | SuccessfulParse a (Q.ByteString IO r)

-- | Apply an attoparsec parser to a streaming bytestring. Return the parsed
-- value and the remaining stream.
parse
  :: forall a r.
     Atto.Parser a
  -> Q.ByteString IO r
  -> IO (ParseResult a r)
parse parser bytes0 =
  Q.nextChunk bytes0 >>= \case
    Left r ->
      pure (EndOfInput r)

    Right (chunk0, bytes1) ->
      let
        loop
          :: Atto.Result a
          -> Q.ByteString IO r
          -> IO (ParseResult a r)
        loop result bytes =
          case result of
            Atto.Fail unconsumed context reason ->
              pure (FailedParse unconsumed context reason)

            Atto.Partial k ->
              Q.nextChunk bytes >>= \case
                Left r ->
                  pure (EndOfInput r)

                Right (chunk, bytes') ->
                  loop (k chunk) bytes'

            Atto.Done leftover x ->
              pure (SuccessfulParse x (Q.chunk leftover *> bytes))
      in
        loop (Atto.parse parser chunk0) bytes1

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
