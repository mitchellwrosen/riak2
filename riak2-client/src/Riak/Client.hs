module Riak.Client
  ( connect
  , send
  , messages
  ) where

import Riak.Message (Message)
import Riak.Request (Request)

import qualified Riak.Message as Message
import qualified Riak.Request as Request

import Control.Exception         (Exception, throwIO)
import Control.Monad             (unless)
import Control.Monad.IO.Class    (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Data.ByteString           (ByteString)
import Data.Function             (fix)
import Network.Socket            (HostName, PortNumber, Socket,
                                  SocketType(Stream))
import Streaming                 (Of, Stream, hoist)

import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Streaming      as Q
import qualified Network.Socket                 as Socket hiding (recv)
import qualified Network.Socket.ByteString      as Socket (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import qualified Streaming.Prelude              as Streaming


connect :: HostName -> PortNumber -> IO Socket
connect host port = do
  info : _ <-
    let
      hints =
        Socket.defaultHints { Socket.addrSocketType = Stream }
    in
      Socket.getAddrInfo (Just hints) (Just host) (Just (show port))

  socket :: Socket <-
    Socket.socket
      (Socket.addrFamily info)
      (Socket.addrSocketType info)
      (Socket.addrProtocol info)

  Socket.connect socket (Socket.addrAddress info)

  pure socket

send :: Request a => Socket -> a -> IO ()
send socket request =
  Socket.sendAll socket (Message.encode (Request.toMessage request))


data MessageParseFailure
  = MessageParseFailure
  { context :: [String]
  , reason  :: String
  } deriving stock (Show)
    deriving anyclass (Exception)

messages :: MonadIO m => Socket -> Stream (Of Message) m ()
messages =
  hoist liftIO . messageStream . socketStream

socketStream ::
     MonadIO m
  => Socket
  -> Q.ByteString m ()
socketStream socket =
  fix $ \loop -> do
    bytes :: ByteString <-
      liftIO (Socket.recv socket 4096)

    unless (ByteString.null bytes) $ do
      Q.chunk bytes
      loop

messageStream :: Q.ByteString IO a -> Stream (Of Message) IO a
messageStream bytes0 =
  lift (parseByteStream Message.parse bytes0) >>= \case
    EndOfInput x ->
      pure x

    FailedParse _unconsumed context reason ->
      liftIO $ throwIO MessageParseFailure
        { context = context
        , reason = reason
        }

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
     (ByteString -> Atto.IResult ByteString a)
  -> Q.ByteString IO r
  -> IO (ParseResult a r)
parseByteStream parse bytes0 =
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
        loop (parse chunk0) bytes1
