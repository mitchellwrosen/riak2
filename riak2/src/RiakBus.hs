module RiakBus
  ( Bus
  , EventHandlers(..)
  , withBus
  , ping
  , exchange
  , stream
  , BusError(..)
  ) where

import Libriak.Connection (ConnectError(..), Connection, ConnectionError(..),
                           Endpoint(..))
import Libriak.Request    (Request(..), encodeRequest)
import Libriak.Response   (DecodeError, EncodedResponse(..), Response(..),
                           decodeResponse, responseDone)

import qualified Libriak.Connection as Connection
import qualified Libriak.Proto      as Proto
import qualified Libriak.Response   as Libriak

import Control.Concurrent.STM
import Control.Foldl          (FoldM(..))
import GHC.TypeLits           (KnownNat)


data Bus
  = Bus
  { connVar :: !(TMVar Connection)
    -- ^ The connection. Starts out full, but once a connection error occurs,
    -- becomes empty permanently.
  , sendLock :: !(MVar ())
    -- ^ Lock acquired during sending a request.
  , doneVarRef :: !(IORef (TMVar ()))
  , handlers :: !EventHandlers
  }

data BusError :: Type where
  -- | The bus is permanently closed.
  BusClosedError :: BusError
  -- | A connection error occurred during a send or receive.
  BusConnectionError :: !ConnectionError -> BusError
  -- | A protobuf decode error occurred.
  BusDecodeError :: !DecodeError -> BusError
  -- | A response with an unexpcected message code was received.
  -- TODO put request/response inside
  BusUnexpectedResponseError :: BusError
  deriving stock (Show)

data EventHandlers
  = EventHandlers
  { onSend :: forall code. Request code -> IO ()
    -- ^ Called just prior to sending a request.
  , onReceive :: forall code. Response code -> IO ()
    -- ^ Called just after receiving a response.
  , onConnectionError :: ConnectionError -> IO ()
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
withBus ::
     Endpoint
  -> EventHandlers
  -> (Bus -> IO a)
  -> IO (Either ConnectError a)
withBus endpoint handlers callback = do
  sendLock :: MVar () <-
    newMVar ()

  doneVarRef :: IORef (TMVar ()) <-
    newIORef =<< newTMVarIO ()

  Connection.withConnection endpoint $ \connection -> do
    connVar :: TMVar Connection <-
      newTMVarIO connection

    callback Bus
      { connVar = connVar
      , sendLock = sendLock
      , doneVarRef = doneVarRef
      , handlers = handlers
      }

ping ::
     Bus
  -> IO (Either BusError (Either (Response 0) (Response 2)))
ping bus =
  exchange bus (ReqRpbPing Proto.defMessage)

withConnection ::
     Bus
  -> (Connection -> IO (Either BusError a))
  -> IO (Either BusError a)
withConnection Bus { connVar, handlers } callback =
  atomically (tryReadTMVar connVar) >>= \case
    Just connection -> do
      callback connection >>= \case
        Left err -> do
          void (atomically (tryTakeTMVar connVar))

          case err of
            BusClosedError ->
              pure ()
            BusConnectionError err ->
              onConnectionError handlers err
            BusDecodeError _ ->
              pure ()
            BusUnexpectedResponseError ->
              pure ()

          pure (Left err)

        Right result ->
          pure (Right result)

    Nothing ->
      pure (Left BusClosedError)

send ::
     EventHandlers
  -> Connection
  -> Request code
  -> IO (Either ConnectionError ())
send handlers connection request = do
  onSend handlers request
  Connection.send connection (encodeRequest request)

receive ::
     forall code.
     KnownNat code
  => EventHandlers
  -> Connection
  -> IO (Either BusError (Either (Response 0) (Response code)))
receive handlers connection =
  Connection.receive connection >>= \case
    Left err ->
      pure (Left (BusConnectionError err))

    Right (Libriak.EncodedResponse bytes) ->
      case decodeResponse (EncodedResponse bytes) of
        Left err -> do
          pure (Left (BusDecodeError err))

        Right response -> do
          either (onReceive handlers) (onReceive handlers) response
          pure (Right response)

-- | Send a request and receive the response (a single message).
--
-- TODO: Handle sooo many race conditions wrt. async exceptions (important use
-- case: killing a thread after a timeout)
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
            (tryReadTMVar connVar >>= \case
              Nothing -> pure (Just BusClosedError)
              Just _ -> retry)

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
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
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
