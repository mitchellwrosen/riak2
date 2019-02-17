-- | A Riak client that manages another client by reconnecting automatically.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Handle.Impl.Managed
  ( Handle
  , HandleConfig(..)
  , ReconnectSettings(..)
  , withHandle
  , exchange
  , stream
  , ManagedHandleCrashed(..)
    -- ** Re-exports
  , ConnectError(..)
  , ConnectionError(..)
  ) where

import Libriak.Connection (ConnectError(..), ConnectionError(..))
import Libriak.Request    (Request)
import Libriak.Response   (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket, catchAny,
                               tryAny)
import Control.Monad          (when)
import Data.Fixed             (Fixed(..))
import Data.Time.Clock        (NominalDiffTime, UTCTime, diffUTCTime,
                               getCurrentTime, nominalDiffTimeToSeconds)
import Numeric.Natural        (Natural)


data Handle
  = Handle
  { handleVar :: !(TMVar (Handle.Handle, Natural))
    -- ^ The handle, and which "generation" it is (when 0 dies, 1 replaces it,
    -- etc.
  , errorVar :: !(TMVar ConnectionError)
    -- ^ The last error some client received when trying to use the handle.
  }

data HandleConfig
  = HandleConfig
  { innerConfig :: !Handle.HandleConfig
    -- ^ The inner handle's config.
  , reconnectSettings ::
      !(Either ConnectError ConnectionError -> Maybe ReconnectSettings)
    -- ^ How to behave when an error occurs. 'Nothing' means don't reconnect.
  , onReconnectAttempt :: !(Either ConnectError ConnectionError -> IO ())
    -- ^ A function called just before a reconnect attempt is made.
  }

data ReconnectSettings
  = ReconnectSettings
  { initialDelay :: !NominalDiffTime
    -- ^ How long to delay before reconnecting for the first time.
  , retryFor :: !NominalDiffTime
    -- ^ Attempt to reconnect periodically until this amount of time has passed.
  }

newtype ManagedHandleCrashed
  = ManagedHandleCrashed SomeException
  deriving stock (Show)

instance Exception ManagedHandleCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

newtype ManagedHandleConnectionError
  = ManagedHandleConnectionError (Either ConnectError ConnectionError)
  deriving stock (Show)

instance Exception ManagedHandleConnectionError where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
--
-- /Throws/. If the background manager thread crashes, which indicates there is
-- a bug in the library, throws an asynchronous 'ManagedHandleCrashed'
-- exception.
--
-- /Throws/. If the background manager thread ends gracefully due to the
-- provided reconnect settings, throws asynchronous
-- 'ManagedHandleConnectionError' with the connection error that ultimately
-- caused the teardown.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either ConnectError a)
withHandle config onSuccess = do
  handleVar :: TMVar (Handle.Handle, Natural) <-
    newEmptyTMVarIO

  errorVar :: TMVar ConnectionError <-
    newEmptyTMVarIO

  threadId :: ThreadId <-
    myThreadId

  bracket
    (forkIOWithUnmask $ \unmask ->
      tryAny (unmask (manager config handleVar errorVar)) >>= \case
        Left err ->
          throwTo threadId (ManagedHandleCrashed err)
        Right err ->
          throwTo threadId (ManagedHandleConnectionError err)
    )
    killThread
    (\_ -> do
      Right <$>
        onSuccess Handle
          { handleVar = handleVar
          , errorVar = errorVar
          })

-- The manager thread:
--
-- * Acquire an underlying connection.
-- * Smuggle it out to the rest of the world via a TMVar.
-- * Wait for an error to appear in another TMVar, then reconnect.
--
-- Meanwhile, users of this handle (via exchange/stream) grab the underlying
-- handle (if available), use it, and if anything goes wrong, write to the error
-- TMVar and retry when a new connection is established.
--
-- Returns with the error that caused the manager thread to ultimately shut
-- down, per the reconnect settings.
manager ::
     HandleConfig
  -> TMVar (Handle.Handle, Natural)
  -> TMVar ConnectionError
  -> IO (Either ConnectError ConnectionError)
manager
     (HandleConfig innerConfig reconnectSettings onReconnectAttempt)
     handleVar
     errorVar =

  loop 0

  where
    loop :: Natural -> IO (Either ConnectError ConnectionError)
    loop !generation = do
      Handle.withHandle innerConfig (runUntilError generation) >>= \case
        Left connectErr -> do
          putStrLn ("Manager thread connect error: " ++ show connectErr)

          case reconnectSettings (Left connectErr) of
            Nothing -> do
              putStrLn "Manager thread shutting down"
              pure (Left connectErr)

            Just (ReconnectSettings initialDelay retryFor) -> do
              sleep 1
              loop (generation+1)

        Right connectionErr -> do
          putStrLn ("Manager thread connection error: " ++ show connectionErr)

          case reconnectSettings (Right connectionErr) of
            Nothing -> do
              putStrLn "Manager thread shutting down"
              pure (Right connectionErr)

            Just (ReconnectSettings initialDelay retryFor) -> do
              sleep 1
              loop (generation+1)

    runUntilError :: Natural -> Handle.Handle -> IO ConnectionError
    runUntilError generation handle = do
      -- Clear out any previous errors, and put the healthy handle for
      -- clients to use
      atomically $ do
        _ <- tryTakeTMVar errorVar
        putTMVar handleVar (handle, generation)

      -- When a client records an error, remove the handle so no more
      -- clients use it (don't bother clearing out the error var yet)
      atomically $ do
        err <- readTMVar errorVar
        _ <- takeTMVar handleVar
        pure err



-- | Send a request and receive the response (a single message).
exchange ::
     Handle
  -> Request
  -> IO (Either ConnectionError Response)
exchange Handle { handleVar, errorVar } request =
  loop 0

  where
    loop :: Natural -> IO (Either ConnectionError Response)
    loop !healthyGen = do
      (handle, gen) <-
        waitForGen healthyGen handleVar

      Handle.exchange handle request >>= \case
        Left err -> do
          -- Notify the manager thread of an error (try put, because it's ok
          -- if we are not the first thread to do so)
          _ <- atomically (tryPutTMVar errorVar err)

          -- Try again once the connection is re-established.
          loop (gen + 1)

        Right response ->
          pure (Right response)

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either ConnectionError r)
stream Handle { handleVar, errorVar } request value step =
  loop 0

  where
    loop :: Natural -> IO (Either ConnectionError r)
    loop !healthyGen = do
      (handle, gen) <-
        waitForGen healthyGen handleVar

      Handle.stream handle request value step >>= \case
        Left err -> do
          -- Notify the manager thread of an error (try put, because it's ok
          -- if we are not the first thread to do so)
          _ <- atomically (tryPutTMVar errorVar err)

          -- Try again once the connection is re-established.
          loop (gen + 1)

        Right response ->
          pure (Right response)

-- Wait for (at least) the given generation of handle.
waitForGen ::
     Natural
  -> TMVar (Handle.Handle, Natural)
  -> IO (Handle.Handle, Natural)
waitForGen healthyGen handleVar =
  atomically $ do
    (handle, gen) <- readTMVar handleVar
    when (gen < healthyGen) retry
    pure (handle, gen)

sleep :: NominalDiffTime -> IO ()
sleep seconds =
  case nominalDiffTimeToSeconds seconds of
    MkFixed picoseconds ->
      threadDelay (fromIntegral (picoseconds `div` 1000000))
