module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , withManagedBus
  , exchange
  , stream
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..), EventHandlers(..))
import RiakDebug          (debug)

import qualified Libriak.Proto as Proto
import qualified RiakBus       as Bus

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket, tryAny)
import Control.Foldl          (FoldM)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, nominalDiffTimeToSeconds)
import GHC.TypeLits           (KnownNat)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TVar Status)
  , handlers :: !EventHandlers
  }

data Status :: Type where
  Connecting :: Status
  Alive :: !Bus -> Status

data ManagedBusError :: Type where
  -- | The bus is currently connecting.
  ManagedBusConnectingError :: ManagedBusError
  -- | A connection error occurred during a send or receive.
  ManagedBusConnectionError :: !ConnectionError -> ManagedBusError
  -- | A protobuf decode error occurred.
  ManagedBusDecodeError :: !DecodeError -> ManagedBusError
  -- | A response with an unexpcected message code was received.
  -- TODO put request/response inside
  ManagedBusUnexpectedResponseError :: ManagedBusError
  deriving stock (Eq, Show)

-- | The bus manager thread crashed, which indicates a bug in this library.
newtype ManagedBusCrashed
  = ManagedBusCrashed SomeException
  deriving stock (Show)

instance Exception ManagedBusCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | Acquire a managed bus.
--
-- /Throws/. This function will never throw an exception.
withManagedBus ::
     Endpoint
  -> EventHandlers
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus endpoint handlers callback = do
  statusVar :: TVar Status <-
    newTVarIO Connecting

  threadId :: ThreadId <-
    myThreadId

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (manager endpoint handlers statusVar)) >>= \case
          Left err ->
            throwTo threadId (ManagedBusCrashed err)
          Right () ->
            pure ()

  let
    release :: ThreadId -> IO ()
    release =
      killThread

  bracket
    acquire
    release
    (\_ ->
      callback ManagedBus
        { statusVar = statusVar
        , handlers = handlers
        })

manager ::
     Endpoint
  -> EventHandlers
  -> TVar Status
  -> IO ()
manager endpoint handlers statusVar =
  reconnect 1 loop

  where
    reconnect ::
         forall r.
         NominalDiffTime
      -> (Bus -> IO r)
      -> IO r
    reconnect seconds callback = do
      result :: Either ConnectError (Maybe r) <-
        Bus.withBus endpoint handlers $ \bus ->
          Bus.exchange bus (ReqRpbPing Proto.defMessage) >>= \case
            Left err -> do
              debug (show err ++ ", sleeping for " ++ show seconds)
              pure Nothing

            Right (Left err) -> do
              debug (show err ++ ", sleeping for " ++ show seconds)
              pure Nothing

            Right (Right _) ->
              Just <$> callback bus

      case result of
        Left err -> do
          debug (show err ++ ", sleeping for " ++ show seconds)
          sleep seconds
          reconnect (seconds * 1.5) callback

        Right Nothing -> do
          sleep seconds
          reconnect (seconds * 1.5) callback

        Right (Just result) ->
          pure result

    loop :: Bus -> IO a
    loop bus = do
      atomically (writeTVar statusVar (Alive bus))

      atomically $ do
        readTVar statusVar >>= \case
          Connecting -> pure ()
          Alive{} -> retry

      reconnect 1 loop

withBus ::
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus ManagedBus { statusVar } callback =
  readTVarIO statusVar >>= \case
    Connecting ->
      pure (Left ManagedBusConnectingError)

    Alive bus ->
      first fromBusError <$>
        callback bus

  where
    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusConnectionError err -> ManagedBusConnectionError err
      BusDecodeError err -> ManagedBusDecodeError err
      BusUnexpectedResponseError -> ManagedBusUnexpectedResponseError


-- | Send a request and receive the response (a single message).
exchange ::
     forall code.
     KnownNat code
  => ManagedBus
  -> Request code
  -> IO (Either ManagedBusError (Either (Response 0) (Response code)))
exchange managedBus request =
  withBus managedBus (\bus -> Bus.exchange bus request)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ code r.
     KnownNat code
  => ManagedBus -- ^
  -> Request code -- ^
  -> FoldM IO (Response code) r
  -> IO (Either ManagedBusError (Either (Response 0) r))
stream managedBus request responseFold =
  withBus managedBus (\bus -> Bus.stream bus request responseFold)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
