{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables #-}

module Riak.Internal.ConcurrentConnection
  ( Connection
  , connect
  , close
  , exchange
  ) where

import Control.Exception (BlockedIndefinitelyOnMVar(..))
import Network.Socket    (AddrInfo(..), HostName, PortNumber, Socket,
                          SocketType(Stream), defaultHints, getAddrInfo)
import Streaming         (Of, Stream)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Lazy           as Lazy (ByteString)
import qualified Data.ByteString.Streaming      as Q
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Streaming.Prelude              as Streaming

import Riak.Internal.Message
import Riak.Internal.Panic
import Riak.Internal.Prelude

-- | A thread-safe connection to Riak.
data Connection
  = Connection
      !Socket
      !(TQueue Lazy.ByteString)
      !(TQueue (Message -> IO ()))
      !ThreadId
      !ThreadId
      !(STM SomeException)

connect :: HostName -> PortNumber -> IO Connection
connect host port = do
  info : _ <-
    let
      hints =
        defaultHints { addrSocketType = Stream }
    in
      getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Socket.socket (addrFamily info) (addrSocketType info) (addrProtocol info)

  Socket.connect socket (addrAddress info)

  sendQueue :: TQueue Lazy.ByteString <-
    newTQueueIO

  recvQueue :: TQueue (Message -> IO ()) <-
    newTQueueIO

  exVar :: TMVar SomeException <-
    newEmptyTMVarIO

  sendThreadId :: ThreadId <-
    forkIOWithUnmask $ \unmask ->
      let
        loop :: IO r
        loop =
          forever $
            atomically (readTQueue sendQueue) >>=
              Socket.sendAll socket
      in
        unmask loop
          `catch` (void . atomically . tryPutTMVar exVar)

  recvThreadId :: ThreadId <-
    forkIOWithUnmask $ \unmask ->
      let
        loop :: Stream (Of Message) IO () -> IO ()
        loop messages =
          Streaming.uncons messages >>= \case
            Nothing ->
              pure ()

            Just (message, messages') -> do
              consume :: Message -> IO () <-
                atomically (readTQueue recvQueue)
              consume message
              loop messages'
      in
        unmask (loop (messageStream (socketStream socket)))
          `catch` (void . atomically . tryPutTMVar exVar)

  pure $ Connection
    socket
    sendQueue
    recvQueue
    sendThreadId
    recvThreadId
    (readTMVar exVar)

close :: Connection -> IO ()
close (Connection socket _ _ sendTid recvTid _) = do
  killThread sendTid
  killThread recvTid
  Socket.close socket

exchange :: Connection -> Message -> (Message -> IO a) -> IO (Maybe a)
exchange (Connection _ sendQueue recvQueue _ _ ex) request consume = do
  resultVar :: MVar a <-
    newEmptyMVar

  payload :: Lazy.ByteString <-
    pure $! encodeMessage request

  atomically $ do
    writeTQueue sendQueue payload
    writeTQueue recvQueue (consume >=> putMVar resultVar)

  (Just <$> takeMVar resultVar)
    `catch` \BlockedIndefinitelyOnMVar ->
      (atomically ex >>= throwIO)

socketStream :: Socket -> Q.ByteString IO ()
socketStream socket =
  fix $ \loop -> do
    bytes :: ByteString <-
      liftIO (Socket.recv socket 4096)
    if ByteString.null bytes
      then pure ()
      else do
        Q.chunk bytes
        loop

messageStream :: Q.ByteString IO a -> Stream (Of Message) IO a
messageStream bytes0 =
  lift (parseByteStream messageParser bytes0) >>= \case
    EndOfInput x ->
      pure x

    FailedParse _unconsumed context reason ->
      panic "Riak parse failure"
        ( ("context", context)
        , ("reason", reason)
        )

    SuccessfulParse message bytes1 -> do
      Streaming.yield message
      messageStream bytes1

-- | Throwaway 'parseByteStream' result type.
data ParseResult a r
  = EndOfInput r
  | FailedParse !ByteString  ![String] !String
  | SuccessfulParse a (Q.ByteString IO r)

-- | Apply an attoparsec parser to a streaming bytestring. Return the parsed
-- value and the remaining stream.
parseByteStream
  :: forall a r.
     Atto.Parser a
  -> Q.ByteString IO r
  -> IO (ParseResult a r)
parseByteStream parser bytes0 =
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
