-- | A Riak client that manages another client by reconnecting automatically.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Handle.Impl.Managed
  ( Handle
  , HandleConfig
  , withHandle
  , exchange
  , stream
  , HandleConnectError
  , HandleError
  ) where

import Libriak.Request  (Request)
import Libriak.Response (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (asyncExceptionFromException,
                               asyncExceptionToException)
import Control.Exception.Safe (Exception(..), SomeException, bracket, catchAny)
import Control.Monad          (when)
import Numeric.Natural        (Natural)


data Handle
  = Handle
  { handleVar :: !(TMVar (Handle.Handle, Natural))
    -- ^ The handle, and which "generation" it is (when 0 dies, 1 replaces it,
    -- etc.
  , errorVar :: !(TMVar Handle.HandleError)
    -- ^ The last error some client received when trying to use the handle.
  }

type HandleConfig
  = Handle.HandleConfig

-- TODO managed handle errors

data HandleError
  deriving stock (Eq, Show)

data HandleConnectError
  deriving stock (Eq, Show)

data HandleCrashed
  = HandleCrashed SomeException
  deriving stock (Show)

instance Exception HandleCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
--
-- /Throws/. If the background manager thread crashes, throws an asynchronous
-- 'HandleCrashed' exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either HandleConnectError a)
withHandle config onSuccess = do
  handleVar :: TMVar (Handle.Handle, Natural) <-
    newEmptyTMVarIO

  errorVar :: TMVar Handle.HandleError <-
    newEmptyTMVarIO

  threadId :: ThreadId <-
    myThreadId

  bracket
    (forkIOWithUnmask $ \unmask ->
      unmask (manager config handleVar errorVar) `catchAny` \e ->
        throwTo threadId (HandleCrashed e))
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
manager ::
     HandleConfig
  -> TMVar (Handle.Handle, Natural)
  -> TMVar Handle.HandleError
  -> IO a
manager config handleVar errorVar =
  loop 0

  where
    loop :: Natural -> IO a
    loop !generation = do
      Handle.withHandle config runUntilError >>= \case
        Left connectErr -> do
          putStrLn ("Manager thread connect error: " ++ show connectErr)
          threadDelay 1000000
          loop (generation+1)

        Right handleErr -> do
          putStrLn ("Manager thread handle error: " ++ show handleErr)
          threadDelay 1000000
          loop (generation+1)

      where
        runUntilError :: Handle.Handle -> IO Handle.HandleError
        runUntilError handle = do
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
  -> IO (Either HandleError Response)
exchange Handle { handleVar, errorVar } request =
  loop 0

  where
    loop :: Natural -> IO (Either HandleError Response)
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
  -> IO (Either HandleError r)
stream Handle { handleVar, errorVar } request value step =
  loop 0

  where
    loop :: Natural -> IO (Either HandleError r)
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
