{-# LANGUAGE AllowAmbiguousTypes #-}

module Riak.Internal.Connection
  ( RiakConnection(..)
  , riakConnect
  , riakDisconnect
  , riakExchange
  , riakExchange_
  , riakStream
  ) where

import Riak.Internal.Debug
import Riak.Internal.Prelude
import Riak.Message          (Message)
import Riak.Proto            (RpbErrorResp)
import Riak.Request          (Request)
import Riak.Response         (Response)

import qualified Riak.Client   as Client
import qualified Riak.Response as Response

import Control.Exception (BlockedIndefinitelyOnMVar(..),
                          BlockedIndefinitelyOnSTM(..))
import Network.Socket    (HostName, PortNumber, Socket)
import Streaming         (Of, Stream)

import qualified Network.Socket    as Socket hiding (recv)
import qualified Streaming         as Streaming
import qualified Streaming.Prelude as Streaming


-- | A thread-safe connection to Riak.
data RiakConnection
  = RiakConnection
      !Client.Socket
      !(MVar ())
      !(TQueue (Stream ((->) Message) IO ()))
      !ThreadId
      !(TMVar SomeException)

data EOF = EOF
  deriving stock (Show)
  deriving anyclass (Exception)

riakConnect :: HostName -> PortNumber -> IO RiakConnection
riakConnect host port = do
  socket :: Client.Socket <-
    Client.connect host port

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
        unmask (loop (Client.messages socket))
          `catch` \ex -> do
            debug ("[riak] recv thread: " ++ show ex)
            void (atomically (tryPutTMVar exVar ex))

  let
    conn :: RiakConnection
    conn =
      RiakConnection socket sem recvQueue recvThreadId exVar

  void (mkWeakMVar sem (riakDisconnect conn))

  pure conn

riakDisconnect :: RiakConnection -> IO ()
riakDisconnect (RiakConnection socket _ _ recvTid _) = do
  killThread recvTid
  Client.close socket

riakSend :: Request a => RiakConnection -> a -> IO ()
riakSend (RiakConnection socket _ _ _ exVar) request =
  Client.send socket request
    `catch` \e -> do
      void (atomically (tryPutTMVar exVar e))
      throwIO e

riakExchange
  :: forall a b.
     (Request a, Response b)
  => RiakConnection
  -> a
  -> IO (Either RpbErrorResp b)
riakExchange conn request = do
  join . fmap sequenceA . Response.parse =<< riakExchange__ conn request

riakExchange_
  :: forall b a.
     (Request a, Response b)
  => RiakConnection
  -> a
  -> IO (Either RpbErrorResp ())
riakExchange_ conn request = do
  fmap (() <$) . Response.parse @b =<< riakExchange__ conn request

riakExchange__
  :: forall a.
     Request a
  => RiakConnection
  -> a
  -> IO Message
riakExchange__ conn@(RiakConnection _ sem recvQueue _ exVar) request = do
  -- debug "[riak] send"
  debug ("[riak] send: " ++ show request)

  resultVar :: MVar Message <-
    newEmptyMVar

  withMVar sem $ \() -> do
    riakSend conn request

    let
      consumer :: Stream ((->) Message) IO ()
      consumer =
        Streaming.wrap (lift . putMVar resultVar)

    atomically (writeTQueue recvQueue consumer)

  takeMVar resultVar `catch`
    \BlockedIndefinitelyOnMVar -> atomically (readTMVar exVar) >>= throwIO

riakStream
  :: forall a b r x.
     (Request a, Response b)
  => RiakConnection -- ^
  -> (b -> Bool) -- ^ Done?
  -> a -- ^
  -> (x -> b -> IO x) -- ^ Step
  -> IO x -- ^ Initial
  -> (x -> IO r) -- ^ Extract
  -> IO (Either RpbErrorResp r)
riakStream
    conn@(RiakConnection _ sem recvQueue _ exVar) done request step initial0
    extract = do
  -- debug "[riak] send"
  -- debug ("[riak] send: " ++ show request)

  responseQueue :: TQueue (Either RpbErrorResp b) <-
    newTQueueIO

  -- Streaming responses are special; when one is active, no other requests can
  -- be serviced on this socket by riak. I learned this the hard way by
  -- reading riak source code.
  --
  -- So, hold a lock on the socket for the entirety of the request-response
  -- exchange, not just during sending the request.
  withMVar sem $ \() -> do
    riakSend conn request

    -- TODO don't bother going to/from the recv thread here, just recv manually

    let
      -- We have to decode the payloads to know when the stream is done, so just
      -- do the decoding on the recv thread.
      consumer :: Stream ((->) Message) IO ()
      consumer =
        Streaming.wrap $ \message -> do
          response :: Either RpbErrorResp b <-
            lift ((join . fmap sequenceA . Response.parse) message)
          -- debug "[riak] recv"
          -- debug ("[riak] recv: " ++ either show show response)

          lift (atomically (writeTQueue responseQueue response))

          unless (either (\_ -> True) done response)
            consumer

    atomically (writeTQueue recvQueue consumer)

    flip fix initial0 $ \loop initial -> do
      response :: Either RpbErrorResp b <-
        atomically (readTQueue responseQueue)
          `catch` \BlockedIndefinitelyOnSTM ->
            (atomically (readTMVar exVar) >>= throwIO)

      case response of
        Left ex ->
          pure (Left ex)

        Right v | done v ->
          Right <$> (initial >>= \x -> step x v >>= extract)

        Right v ->
          loop (initial >>= \x -> step x v)

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
