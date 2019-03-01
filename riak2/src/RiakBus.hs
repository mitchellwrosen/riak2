module RiakBus
  ( Bus
  , withBus
  , exchange
  , stream
  , BusError(..)
  ) where

import Libriak.Connection (ConnectError(..), Connection, ConnectionError(..),
                           DecodeError, Endpoint(..))
import Libriak.Request    (Request, encodeRequest)
import Libriak.Response   (Response(..), decodeResponse)

import qualified Libriak.Connection as Connection
import qualified Libriak.Proto      as Proto

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Foldl           (FoldM(..))
import Control.Lens            ((^.))


data Bus
  = Bus
  { statusVar :: !(TVar Status)
  , sendLock :: !(MVar ())
    -- ^ Lock acquired during sending a request.
  , doneVarRef :: !(IORef (TMVar ()))
  }

-- | The connection status - it's alive until something goes wrong.
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

-- | Acquire a bus.
--
-- /Throws/: This function will never throw an exception.
withBus ::
     Endpoint
  -> (Bus -> IO a)
  -> IO (Either ConnectError a)
withBus endpoint callback = do
  sendLock :: MVar () <-
    newMVar ()

  doneVarRef :: IORef (TMVar ()) <-
    newIORef =<< newEmptyTMVarIO

  Connection.withConnection endpoint $ \connection -> do
    statusVar :: TVar Status <-
      newTVarIO (Alive connection)

    callback Bus
      { statusVar = statusVar
      , sendLock = sendLock
      , doneVarRef = doneVarRef
      }

withConnection ::
     Bus
  -> (Connection -> IO (Either BusError a))
  -> IO (Either BusError a)
withConnection bus callback =
  readTVarIO (statusVar bus) >>= \case
    Alive connection ->
      callback connection

    Dead err ->
      pure (Left err)

receive ::
     (Response -> Maybe a)
  -> Connection
  -> IO (Either BusError (Either ByteString a))
receive parse connection =
  Connection.receive connection >>= \case
    Left err ->
      pure (Left (BusConnectionError err))

    Right bytes ->
      case decodeResponse bytes of
        Left err ->
          pure (Left (BusDecodeError err))

        Right response ->
          case response of
            RespRpbError err ->
              pure (Right (Left (err ^. Proto.errmsg)))

            _ ->
              case parse response of
                Nothing ->
                  pure (Left BusUnexpectedResponseError)

                Just response ->
                  pure (Right (Right response))

-- | Send a request and receive the response (a single message).
--
-- TODO: Handle sooo many race conditions wrt. async exceptions (important use
-- case: killing a thread after a timeout)
--
-- TODO: who puts Dead to the status, and how?
exchange ::
     Bus -- ^
  -> Request -- ^
  -> (Response -> Maybe a) -- ^
  -> IO (Either BusError (Either ByteString a))
exchange bus@(Bus { statusVar, sendLock, doneVarRef }) request parse =
  withConnection bus $ \connection -> do
    -- Try sending, which either results in an error, or two empty TMVars: one
    -- that will fill when it's our turn to receive, and one that we must fill
    -- when we are done receiving.
    sendResult :: Either BusError (TMVar (), TMVar ()) <-
      withMVar sendLock $ \() ->
        Connection.send connection (encodeRequest request) >>= \case
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
            receive parse connection >>= \case
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
     ∀ a r.
     Proto.HasLens' a "done" Bool
  => Bus -- ^
  -> Request -- ^
  -> (Response -> Maybe a)
  -> FoldM IO a r
  -> IO (Either BusError (Either ByteString r))
stream bus@(Bus { sendLock }) request parse (FoldM step (initial :: IO x) extract) =
  withConnection bus $ \connection ->
    -- Riak request handling state machine is odd. Streaming responses are
    -- special; when one is active, no other requests can be serviced on this
    -- socket.
    --
    -- So, hold a lock for the entirety of the request-response exchange, not
    -- just during sending the request.
    withMVar sendLock $ \() ->
      Connection.send connection (encodeRequest request) >>= \case
        Left err ->
          pure (Left (BusConnectionError err))

        Right () ->
          let
            consume :: x -> IO (Either BusError (Either ByteString r))
            consume value =
              receive parse connection >>= \case
                Left err ->
                  pure (Left err)

                Right (Left err) ->
                  pure (Right (Left err))

                Right (Right response) -> do
                  newValue <- step value response
                  if response ^. Proto.done
                    then Right . Right <$> extract newValue
                    else consume newValue
          in
            consume =<< initial
