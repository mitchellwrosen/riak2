{-# LANGUAGE DeriveAnyClass, DerivingStrategies, LambdaCase, NoImplicitPrelude,
             OverloadedStrings, ScopedTypeVariables #-}

module Riak.Internal.Connection
  ( RiakConnection(..)
  , riakConnect
  , riakDisconnect
  , riakExchange
  , riakStream
  ) where

import Control.Exception (BlockedIndefinitelyOnMVar(..),
                          BlockedIndefinitelyOnSTM(..))
import Network.Socket    (AddrInfo(..), HostName, PortNumber, Socket,
                          SocketType(Stream), defaultHints, getAddrInfo)
import Streaming         (Of, Stream)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Streaming      as Q
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Streaming                      as Streaming
import qualified Streaming.Prelude              as Streaming

import Riak.Internal.Debug
import Riak.Internal.Message
import Riak.Internal.Panic
import Riak.Internal.Prelude
import Riak.Internal.Request
import Riak.Internal.Response
import Riak.Internal.Types

-- | A thread-safe connection to Riak.
data RiakConnection
  = RiakConnection
      !Socket
      !(MVar ())
      !(TQueue (Stream ((->) Message) IO ()))
      !ThreadId
      !(TMVar SomeException)

data EOF = EOF
  deriving stock (Show)
  deriving anyclass (Exception)

riakConnect :: HostName -> PortNumber -> IO RiakConnection
riakConnect host port = do
  info : _ <-
    let
      hints =
        defaultHints { addrSocketType = Stream }
    in
      getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Socket.socket (addrFamily info) (addrSocketType info) (addrProtocol info)

  Socket.connect socket (addrAddress info)

  sem :: MVar () <-
    newMVar ()

  recvQueue :: TQueue (Stream ((->) Message) IO ()) <-
    newTQueueIO

  exVar :: TMVar SomeException <-
    newEmptyTMVarIO

  recvThreadId :: ThreadId <-
    forkIOWithUnmask $ \unmask ->
      let
        loop :: Stream (Of Message) IO () -> IO ()
        loop messages = do
          consumer :: Stream ((->) Message) IO () <-
            atomically (readTQueue recvQueue)

          feed messages consumer >>= \case
            Nothing ->
              throwIO EOF

            Just messages' ->
              loop messages'

      in
        unmask (loop (messageStream (socketStream socket)))
          `catch` (void . atomically . tryPutTMVar exVar)

  let
    conn :: RiakConnection
    conn =
      RiakConnection socket sem recvQueue recvThreadId exVar

  void (mkWeakMVar sem (riakDisconnect conn))

  pure conn

riakDisconnect :: RiakConnection -> IO ()
riakDisconnect (RiakConnection socket _ _ recvTid _) = do
  killThread recvTid
  Socket.close socket

riakSend :: Request a => RiakConnection -> a -> IO ()
riakSend (RiakConnection socket _ _ _ exVar) request =
  Socket.sendAll socket (encodeMessage (requestToMessage request))
    `catch` \e -> do
      void (atomically (tryPutTMVar exVar e))
      throwIO e

riakExchange
  :: forall a b.
     (Request a, Response b)
  => RiakConnection
  -> a
  -> IO (Either RiakError b)
riakExchange conn@(RiakConnection _ sem recvQueue _ exVar) request = do
  resultVar :: MVar Message <-
    newEmptyMVar

  debug (">>> " ++ show request)

  withMVar sem $ \() -> do
    riakSend conn request

    let
      consumer :: Stream ((->) Message) IO ()
      consumer =
        Streaming.wrap (lift . putMVar resultVar)

    atomically (writeTQueue recvQueue consumer)

  let
    recv :: IO (Either RiakError b)
    recv = do
      result :: Either RiakError b <-
        parseResponse =<< takeMVar resultVar
      debug ("<<< " ++ either show show result)
      pure result

  recv `catch`
    \BlockedIndefinitelyOnMVar -> atomically (readTMVar exVar) >>= throwIO

riakStream
  :: forall a b.
     (Request a, Response b)
  => RiakConnection -- ^
  -> (b -> Bool) -- ^ Done?
  -> a -- ^
  -> ListT (ExceptT RiakError IO) b
riakStream conn@(RiakConnection _ sem recvQueue _ exVar) done request = do
  responseQueue :: TQueue (Either RiakError b) <-
    liftIO newTQueueIO

  liftIO . withMVar sem $ \() -> do
    riakSend conn request

    let
      -- We have to decode the payloads to know when the stream is done, so just
      -- do the decoding on the recv thread.
      consumer :: Stream ((->) Message) IO ()
      consumer =
        Streaming.wrap $ \message -> do
          response :: Either RiakError b <-
            lift (parseResponse message)

          lift (atomically (writeTQueue responseQueue response))

          unless (either (\_ -> True) done response)
            consumer

    atomically (writeTQueue recvQueue consumer)

  fix $ \loop -> do
    response :: b <-
      (lift . ExceptT)
        (atomically (readTQueue responseQueue)
          `catch` \BlockedIndefinitelyOnSTM ->
            (atomically (readTMVar exVar) >>= throwIO))

    if done response
      then pure response
      else pure response <|> loop

socketStream :: Socket -> Q.ByteString IO ()
socketStream socket =
  fix $ \loop -> do
    bytes :: ByteString <-
      liftIO (Socket.recv socket 4096)
    unless (ByteString.null bytes) $ do
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

feed
  :: Monad m
  => Stream (Of a) m ()
  -> Stream ((->) a) m ()
  -> m (Maybe (Stream (Of a) m ()))
feed xs fs =
  Streaming.inspect fs >>= \case
    Left _ ->
      pure (Just xs)

    Right f ->
      Streaming.next xs >>= \case
        Left _ ->
          pure Nothing

        Right (x, xs') ->
          feed xs' (f x)
