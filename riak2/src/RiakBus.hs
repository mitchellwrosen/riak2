module RiakBus
  ( Bus
  , EventHandlers(..)
  , withBus
  , exchange
  , stream
  , BusError(..)
  ) where

import Libriak.Connection (ConnectError(..), Connection, ConnectionError(..),
                           Endpoint(..))
import Libriak.Request    (Request, encodeRequest)
import Libriak.Response   (DecodeError, EncodedResponse(..), Response(..),
                           decodeResponse, responseDone)
import RiakDebug          (debug)

import qualified Libriak.Connection as Connection
import qualified Libriak.Response   as Libriak

import Control.Concurrent.STM
import Control.Foldl          (FoldM(..))
import GHC.TypeLits           (KnownNat)


data Bus
  = Bus
  { statusVar :: !(TVar Status)
  , sendLock :: !(MVar ())
    -- ^ Lock acquired during sending a request.
  , doneVarRef :: !(IORef (TMVar ()))
  , handlers :: !EventHandlers
  }

-- | The connection status - it's alive until something goes wrong. Subsequent
-- attempts to use the bus will continue returning the same 'BusError' that
-- brought it down initially.
data Status :: Type where
  Alive :: !Connection -> Status
  Dead :: !BusError -> Status

data BusError :: Type where
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
    statusVar :: TVar Status <-
      newTVarIO (Alive connection)

    callback Bus
      { statusVar = statusVar
      , sendLock = sendLock
      , doneVarRef = doneVarRef
      , handlers = handlers
      }

withConnection ::
     Bus
  -> (Connection -> IO (Either BusError a))
  -> IO (Either BusError a)
withConnection Bus { handlers, statusVar } callback =
  readTVarIO statusVar >>= \case
    Alive connection ->
      callback connection >>= \case
        Left err -> do
          atomically (writeTVar statusVar (Dead err))
          case err of
            BusConnectionError err ->
              onConnectionError handlers err
            BusDecodeError err ->
              debug ("bus decode error: " ++ show err)
            BusUnexpectedResponseError ->
              debug "bus unexpected response error"

          pure (Left err)

        Right result ->
          pure (Right result)

    Dead err ->
      pure (Left err)

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
exchange bus@(Bus { statusVar, sendLock, doneVarRef, handlers }) request =
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
        -- It's a race: either something goes wrong somewhere (at which point
        -- the connection status var will be filled with a bus error), or
        -- everything goes well and it becomes our turn to receive.
        waitResult :: Maybe BusError <-
          atomically $ do
            (Nothing <$ readTMVar prevDoneVar)
            <|>
            (readTVar statusVar >>= \case
              Alive _ -> retry
              Dead err -> pure (Just err))

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
