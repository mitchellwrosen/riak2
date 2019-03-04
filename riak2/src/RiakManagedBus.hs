-- |
--
-- Managed bus state machine:
--
-- [DISCONNECTED]
--   Request comes in ==> [CONNECTING]
--
-- [CONNECTING]
--   Connect failure ==> [CONNECTING]
--   Connect success ==> [UNHEALTHY]
--
-- [UNHEALTHY]
--   Ping failure ==> [DRAINING]
--   Ping nack ==> [UNHEALTHY]
--   Ping ack ==> [HEALTHY]
--
-- [HEALTHY]
--   Request failure ==> [DRAINING]
--   Ping nack ==> [UNHEALTHY]
--   Idle timeout
--     No in-flight requests ==> [DRAINING]
--     In-flight request(s) ==> [HEALTHY]
--
-- [DRAINING]
--   No in-flight requests ==> [CONNECTING]

module RiakManagedBus
  ( ManagedBus
  , ManagedBusError(..)
  , EventHandlers(..)
  , managedBusReady
  , withManagedBus
  , withBus
  , ManagedBusCrashed(..)
  ) where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request(..))
import Libriak.Response   (DecodeError, Response)
import RiakBus            (Bus, BusError(..))
import RiakDebug          (debug)

import qualified RiakBus as Bus

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception        (asyncExceptionFromException,
                                 asyncExceptionToException)
import Control.Exception.Safe   (Exception(..), SomeException, bracket, bracket_, tryAny)
import Data.Fixed               (Fixed(..))
import Data.Time.Clock          (NominalDiffTime, nominalDiffTimeToSeconds)


data ManagedBus
  = ManagedBus
  { statusVar :: !(TVar Status)
  , inFlightVar :: !(TVar Int)
    -- ^ The number of in-flight requests.
  , lastUsedRef :: !(IORef Word64)
    -- ^ The last time the bus was used.
  , handlers :: !EventHandlers
  }

data Status :: Type where
  -- | * Disconnected due to never being used
  --   * Disconnected due to idle timeout
  --   * Connected but haven't pinged once yet
  --   * Connected but unhealthy (pings began failing)
  Disconnected ::
       !(STM ()) -- "Please connect"
    -> Status

  -- | Connected and healthy.
  Healthy ::
       !Bus
    -> !(STM ()) -- Idle timeout
    -> !(STM ()) -- Request failed
    -> Status

data ManagedBusError :: Type where
  -- | The bus is not ready to accept requests, either because it is connecting,
  -- unhealthy, or draining connections and about to reconnect.
  ManagedBusDisconnectedError :: ManagedBusError
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

data EventHandlers
  = EventHandlers
  { onSend :: !(forall code. Request code -> IO ())
    -- ^ Called just prior to sending a request.
  , onReceive :: !(forall code. Response code -> IO ())
    -- ^ Called just after receiving a response.
  , onConnectError :: !(ConnectError -> IO ())
  , onConnectionError :: !(ConnectionError -> IO ())
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 c1 d1 <> EventHandlers a2 b2 c2 d2 =
    EventHandlers (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)


-- | An STM action that returns when the managed bus is connected and healthy.
managedBusReady :: ManagedBus -> STM ()
managedBusReady ManagedBus { statusVar } =
  readTVar statusVar >>= \case
    Disconnected _ -> retry
    Healthy _ _ _ -> pure ()

-- | Acquire a managed bus.
--
-- /Throws/. This function will never throw an exception.
withManagedBus ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus endpoint receiveTimeout handlers callback = do
  connectVar :: TPing <-
    newTPing

  statusVar :: TVar Status <-
    newTVarIO (Disconnected (ping connectVar))

  inFlightVar :: TVar Int <-
    newTVarIO 0

  lastUsedRef :: IORef Word64 <-
    newIORef =<< getMonotonicTimeNSec

  threadId :: ThreadId <-
    myThreadId

  let
    bus :: ManagedBus
    bus =
      ManagedBus
        { statusVar = statusVar
        , inFlightVar = inFlightVar
        , lastUsedRef = lastUsedRef
        , handlers = handlers
        }

  let
    acquire :: IO ThreadId
    acquire =
      forkIOWithUnmask $ \unmask ->
        tryAny (unmask (managerThread endpoint receiveTimeout connectVar bus)) >>= \case
          Left err ->
            throwTo threadId (ManagedBusCrashed err)
          Right void ->
            absurd void

  bracket
    acquire
    killThread
    (\_ -> callback bus)

managerThread ::
     Endpoint
  -> Int
  -> TPing
  -> ManagedBus
  -> IO Void
managerThread
    endpoint
    receiveTimeout
    connectVar
    ManagedBus { handlers, inFlightVar, lastUsedRef, statusVar } =

  disconnected connectVar

  where
    connecting :: NominalDiffTime -> IO Void
    connecting seconds0 = do
      debug "handle: connecting"
      loop seconds0

      where
        loop :: NominalDiffTime -> IO Void
        loop seconds = do
          result :: Either ConnectError (IO Void) <-
            Bus.withBus endpoint receiveTimeout busHandlers $ \bus -> do
              timeoutVar :: TPing <-
                newTPing

              requestFailedVar :: TPing <-
                newTPing

              unhealthy bus timeoutVar requestFailedVar 1

          case result of
            Left err -> do
              void (tryAny (onConnectError handlers err))
              debug ("handle: " ++ show err)
              sleep seconds
              loop (seconds * 1.5)

            Right next ->
              next

    healthy ::
         Bus
      -> TPing -- Idle timeout
      -> TPing -- Request failed
      -> IO (IO Void)
    healthy bus timeoutVar requestFailedVar = do
      debug "handle: healthy"

      unhealthyVar :: TPing <-
        newTPing

      join .
        withAsync (healthCheckThread (ping unhealthyVar) (ping requestFailedVar) bus) $ \_ ->
        withAsync (idleTimeoutThread (ping timeoutVar) inFlightVar lastUsedRef) $ \_ ->
          atomically (asum
            [ recvPing timeoutVar $> pure (do
                connectVar <- newTPing
                atomically (writeTVar statusVar (Disconnected (ping connectVar)))
                debug "handle: disconnected"
                disconnected connectVar)

            , recvPing unhealthyVar $> do
                atomically (writeTVar statusVar (Disconnected (pure ())))
                debug "handle: unhealthy"
                unhealthy bus timeoutVar requestFailedVar 1

            , recvPing requestFailedVar $> do
                atomically (writeTVar statusVar (Disconnected (pure ())))
                draining
            ])

    unhealthy ::
         Bus
      -> TPing -- Idle timeout
      -> TPing -- Request failed
      -> NominalDiffTime
      -> IO (IO Void)
    unhealthy bus timeoutVar requestFailedVar =
      loop

      where
        loop :: NominalDiffTime -> IO (IO Void)
        loop seconds = do
          Bus.ping bus >>= \case
            Left err -> do
              debug ("handle: " ++ show err)
              sleep seconds
              draining

            Right (Left err) -> do
              debug ("handle: " ++ show err)
              sleep seconds
              loop (seconds * 1.5)

            Right (Right _) -> do
              atomically
                (writeTVar
                  statusVar
                    (Healthy bus (ping timeoutVar) (ping requestFailedVar)))

              healthy bus timeoutVar requestFailedVar

    draining :: IO (IO Void)
    draining = do
      debug "handle: draining"

      atomically $
        readTVar inFlightVar >>= \case
          0 -> pure ()
          _ -> retry

      pure (connecting 1)

    disconnected :: TPing -> IO Void
    disconnected connectVar = do
      atomically (recvPing connectVar)
      connecting 1

    busHandlers :: Bus.EventHandlers
    busHandlers =
      Bus.EventHandlers
        { Bus.onSend = onSend handlers
        , Bus.onReceive = onReceive handlers
        , Bus.onConnectionError = onConnectionError handlers
        }

healthCheckThread ::
     STM () -- Unhealthy
  -> STM () -- Request failed
  -> Bus
  -> IO ()
healthCheckThread unhealthy requestFailed bus =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable ping frequency
      threadDelay (1*1000*1000)

      Bus.ping bus >>= \case
        Left err -> do
          debug ("handle: health check failed: " ++ show err)
          atomically requestFailed

        Right (Left err) -> do
          debug ("handle: health check failed: " ++ show err)
          atomically unhealthy

        Right (Right _) ->
          loop

idleTimeoutThread ::
     STM ()
  -> TVar Int
  -> IORef Word64
  -> IO ()
idleTimeoutThread timeout inFlightVar lastUsedRef =
  loop

  where
    loop :: IO ()
    loop = do
      -- TODO configurable idle timeout
      threadDelay (5*1000*1000)

      now <- getMonotonicTimeNSec
      lastUsed <- readIORef lastUsedRef

      if now - lastUsed > (10*1000*1000*1000)
        then do
          next :: IO () <-
            atomically $
              readTVar inFlightVar >>= \case
                -- We have no in-flight requests, and the idle timeout has
                -- elapsed.
                0 -> do
                  timeout
                  pure (pure ())

                -- We have an in-flight request, so even though the idle timeout
                -- has elapsed, just pretend it hasn't.
                _ ->
                  pure loop

          next
        else
          loop

withBus ::
     forall a.
     ManagedBus
  -> (Bus -> IO (Either BusError a))
  -> IO (Either ManagedBusError a)
withBus
    ManagedBus { inFlightVar, lastUsedRef, statusVar }
    callback =

  join . atomically $
    readTVar statusVar >>= \case
      Disconnected connect -> do
        connect
        pure (pure (Left ManagedBusDisconnectedError))

      Healthy bus _timeout requestFailed -> pure $ do
        writeIORef lastUsedRef =<< getMonotonicTimeNSec

        -- It's ok that this code is not exception-safe. If a callback results
        -- in a bus error, and then we are killed before writing 'Connecting' to
        -- the status var, the next thread that comes along will attempt to use
        -- the dead connection and see a similar bus error.
        doCallback bus >>= \case
          Left err -> do
            atomically requestFailed
            pure (Left (fromBusError err))
          Right result ->
            pure (Right result)

  where
    doCallback :: Bus -> IO (Either BusError a)
    doCallback bus =
      bracket_
        (atomically (modifyTVar' inFlightVar (+1)))
        (atomically (modifyTVar' inFlightVar (subtract 1)))
        (callback bus)

    fromBusError :: BusError -> ManagedBusError
    fromBusError = \case
      BusClosedError -> ManagedBusDisconnectedError
      BusConnectionError err -> ManagedBusConnectionError err
      BusDecodeError err -> ManagedBusDecodeError err
      BusUnexpectedResponseError -> ManagedBusUnexpectedResponseError


newtype TPing
  = TPing (TMVar ())

newTPing :: IO TPing
newTPing =
  TPing <$> newEmptyTMVarIO

ping :: TPing -> STM ()
ping (TPing var) =
  void (tryPutTMVar var ())

recvPing :: TPing -> STM ()
recvPing (TPing var) =
  readTMVar var

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
