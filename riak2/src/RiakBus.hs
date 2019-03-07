{-# LANGUAGE CPP #-}

-- TODO perhaps don't ruin the whole bus if one thread, during an exchange, gets
-- killed before it receives?

module RiakBus
  ( Bus
  , EventHandlers(..)
  , connect
  , disconnect
  , ping
  , exchange
  , stream
  , BusError(..)
  ) where

import Libriak.Connection (CloseException(..), ConnectException(..), Connection,
                           ConnectionError(..), Endpoint(..),
                           Interruptibility(..))
import Libriak.Request    (Request(..), encodeRequest)
import Libriak.Response   (DecodeError, Response(..), decodeResponse,
                           responseDone)

import qualified Libriak.Connection as Connection

import Control.Concurrent.STM
import Control.Exception.Safe (catchAsync, throwIO)
import Control.Foldl          (FoldM(..))
import GHC.TypeLits           (KnownNat)

#ifdef CHAOS
import System.Random (randomIO)
#endif

import qualified Data.Riak.Proto as Proto


data Bus
  = Bus
  { connVar :: !(TVar (Either Connection Connection))
    -- ^ The connection. Starts out on the Right, but once a connection error
    -- occurs, gets put on the Left permanently.
  , sendLock :: !(MVar ())
    -- ^ Lock acquired during sending a request.
  , doneVarRef :: !(IORef (TMVar ()))
  , handlers :: !EventHandlers
  }

instance Eq Bus where
  bus1 == bus2 =
    sendLock bus1 == sendLock bus2

data BusError :: Type where
  -- | The bus is permanently closed.
  BusClosedError :: BusError
  -- | A connection error occurred during a send or receive.
  BusConnectionError :: !ConnectionError -> BusError
  -- | A protobuf decode error occurred.
  BusDecodeError :: !DecodeError -> BusError
  deriving stock (Show)

data EventHandlers
  = EventHandlers
  { onSend :: !(forall code. Request code -> IO ())
    -- ^ Called just prior to sending a request.
  , onReceive :: !(forall code. Response code -> IO ())
    -- ^ Called just after receiving a response.
  , onConnectionError :: !(ConnectionError -> IO ())
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 <> EventHandlers a2 b2 c2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- | Acquire a bus.
--
-- /Throws/: This function will never throw an exception.
connect ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO (Either (ConnectException 'Uninterruptible) Bus)
connect endpoint receiveTimeout handlers =
  Connection.connect endpoint receiveTimeout >>= \case
    Left err ->
      pure (Left err)

    Right connection -> do
      connVar :: TVar (Either Connection Connection) <-
        newTVarIO (Right connection)

      sendLock :: MVar () <-
        newMVar ()

      doneVarRef :: IORef (TMVar ()) <-
        newIORef =<< newTMVarIO ()

      pure (Right Bus
        { connVar = connVar
        , sendLock = sendLock
        , doneVarRef = doneVarRef
        , handlers = handlers
        })

disconnect ::
     Bus
  -> IO (Either CloseException ())
disconnect Bus { connVar } =
  readTVarIO connVar >>=
    either Connection.disconnect Connection.disconnect

ping ::
     Bus
  -> IO (Either BusError (Either (Response 0) (Response 2)))
ping bus =
  exchange bus (ReqRpbPing Proto.defMessage)

withConnection ::
     forall a.
     Bus
  -> (Connection -> IO (Either BusError a))
  -> IO (Either BusError a)
withConnection Bus { connVar, handlers } callback =
  readTVarIO connVar >>= \case
    Left _ ->
      pure (Left BusClosedError)

    Right connection ->
      doCallback connection >>= \case
        Left err -> do
          markAsUnusable

          case err of
            BusClosedError ->
              pure ()
            BusConnectionError err ->
              onConnectionError handlers err
            BusDecodeError _ ->
              pure ()

          pure (Left err)

        Right result ->
          pure (Right result)

  where
    doCallback :: Connection -> IO (Either BusError a)
    doCallback connection =
      callback connection `catchAsync` \ex -> do
        markAsUnusable
        throwIO (ex :: SomeException)

    markAsUnusable :: IO ()
    markAsUnusable =
      atomically $
        readTVar connVar >>= \case
          Left _ -> pure ()
          Right conn -> writeTVar connVar (Left conn)

send ::
     EventHandlers
  -> Connection
  -> Request code
  -> IO (Either ConnectionError ())
send handlers connection request = do
  onSend handlers request

#ifdef CHAOS

  pct :: Double <-
    randomIO

  case pct of
    _ | pct < 0.0025 -> pure (Left LocalShutdown)
      | pct < 0.0050 -> pure (Left RemoteReset)
      | pct < 0.0075 -> pure (Left RemoteShutdown)
      | pct < 0.0100 -> pure (Left RemoteTimeout)
      | otherwise    -> doSend

#else

  doSend

#endif

  where
    doSend :: IO (Either ConnectionError ())
    doSend =
      Connection.send connection (encodeRequest request)

receive ::
     forall code.
     KnownNat code
  => EventHandlers
  -> Connection
  -> IO (Either BusError (Either (Response 0) (Response code)))
receive handlers connection = do

#ifdef CHAOS

  pct :: Double <-
    randomIO

  case pct of
    _ | pct < 0.0025 -> pure (Left (BusConnectionError LocalShutdown))
      | pct < 0.0050 -> pure (Left (BusConnectionError RemoteReset))
      | pct < 0.0075 -> pure (Left (BusConnectionError RemoteShutdown))
      | pct < 0.0100 -> do
                          threadDelay (1*1000*1000)
                          pure (Left (BusConnectionError RemoteTimeout))
      | otherwise    ->
          doRecv

#else

  doRecv

#endif

  where
    doRecv :: IO (Either BusError (Either (Response 0) (Response code)))
    doRecv =
      Connection.receive connection >>= \case
        Left err ->
          pure (Left (BusConnectionError err))

        Right bytes ->
          case decodeResponse bytes of
            Left err -> do
              pure (Left (BusDecodeError err))

            Right response -> do
              either (onReceive handlers) (onReceive handlers) response
              pure (Right response)

-- | Send a request and receive the response (a single message).
exchange ::
     KnownNat code
  => Bus -- ^
  -> Request code -- ^
  -> IO (Either BusError (Either (Response 0) (Response code)))
exchange bus@(Bus { connVar, sendLock, doneVarRef, handlers }) request =
  withConnection bus $ \connection -> do
    -- Try sending, which either results in an error, or two empty TMVars: one
    -- that will fill when it's our turn to receive, and one that we must fill
    -- when we are done receiving.
    sendResult :: Either BusError (TMVar (), TMVar ()) <-
      withMVar sendLock $ \() ->
        send handlers connection request >>= \case
          Left err ->
            pure (Left (BusConnectionError err))

          Right () -> do
            doneVar <- newEmptyTMVarIO
            prevDoneVar <- readIORef doneVarRef
            writeIORef doneVarRef doneVar
            pure (Right (prevDoneVar, doneVar))

    case sendResult of
      Left err ->
        pure (Left err)

      Right (prevDoneVar, doneVar) -> do
        -- It's a race: either a previous request fails, or everything goes well
        -- and it finally becomes our turn to receive.
        waitResult :: Maybe BusError <-
          atomically $ do
            (Nothing <$ readTMVar prevDoneVar)
            <|>
            (readTVar connVar >>= \case
              Left _ -> pure (Just BusClosedError)
              Right _ -> retry)

        case waitResult of
          Nothing ->
            receive handlers connection >>= \case
              Left err ->
                pure (Left err)

              Right response -> do
                atomically (putTMVar doneVar ())
                pure (Right response)

          Just err ->
            pure (Left err)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => Bus -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either BusError (Either (Response 0) r))
stream bus@(Bus { sendLock, handlers }) request (FoldM step (initial :: IO x) extract) =
  withConnection bus $ \connection ->
    -- Riak request handling state machine is odd. Streaming responses are
    -- special; when one is active, no other requests can be serviced on this
    -- socket.
    --
    -- So, hold a lock for the entirety of the request-response exchange, not
    -- just during sending the request.
    withMVar sendLock $ \() ->
      send handlers connection request >>= \case
        Left err ->
          pure (Left (BusConnectionError err))

        Right () ->
          let
            consume :: x -> IO (Either BusError (Either (Response 0) r))
            consume value =
              receive handlers connection >>= \case
                Left err ->
                  pure (Left err)

                Right (Left err) ->
                  pure (Right (Left err))

                Right (Right response) -> do
                  newValue <- step value response
                  if responseDone response
                    then Right . Right <$> extract newValue
                    else consume newValue
          in
            consume =<< initial
