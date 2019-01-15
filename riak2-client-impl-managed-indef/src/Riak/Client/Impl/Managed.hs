-- The managed client.
--
-- * The socket status ("disconnected", "connecting", "connected") is kept in a
--   TVar.
--
-- * Before using the socket, clients check its status.
--
--   * If "disconnected", that means "connect" must be called first, so this
--     operation just fails.
--
--   * If "connecting", the client waits until "connected".
--
--   * If "connected", the client uses the socket. If this operation fails, it
--     sets the status to "connecting" and retries.
--
-- * Meanwhile, a background thread is spawned when 'connect' is called. This
--   background thread waits until the socket status is set to "connecting",
--   then tries to reconnect once per second. When this succeeds, it updates the
--   status to "connected". If the status is set to "disconnected" the
--   background thread quits gracefully.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Client.Impl.Managed
  ( Client
  , new
  , connect
  , disconnect
  , exchange
  , stream
  ) where

import Riak.Message (Message)

import qualified Riak.Client.Signature as Client

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad          (void, when)
import System.IO.Unsafe       (unsafeInterleaveIO)
import UnliftIO.Exception


data Client
  = Client
  { inner :: !Client.Client
  , statusVar :: !(TVar Status)
  , backgroundThreadVar :: !(MVar ThreadId)
  }

data Status
  = Disconnected
  | Connecting
  | Connected

new ::
     Client.Client
  -> IO Client
new inner =
  Client
    <$> pure inner
    <*> newTVarIO Disconnected
    <*> newEmptyMVar

connect :: Client -> IO ()
connect =
  spawnBackgroundThreadIfNotRunning

spawnBackgroundThreadIfNotRunning :: Client -> IO ()
spawnBackgroundThreadIfNotRunning client = do
  -- TODO background thread lifetime/exception handling
  backgroundThread :: ThreadId <-
    unsafeInterleaveIO
      (forkIO
        (runBackgroundThread (inner client) (statusVar client)))

  success :: Bool <-
    tryPutMVar
      (backgroundThreadVar client)
      backgroundThread

  when success (void (evaluate backgroundThread))

runBackgroundThread :: Client.Client -> TVar Status -> IO ()
runBackgroundThread client statusVar =
  loop

  where
    loop :: IO ()
    loop = do
      continue :: Bool <-
        waitForConnecting

      when continue $ do
        Client.disconnect client `catchAny` \_ -> pure ()
        reconnect

    waitForConnecting :: IO Bool
    waitForConnecting = do
      atomically $
        readTVar statusVar >>= \case
          Disconnected -> pure False
          Connecting   -> pure True
          Connected    -> retry

    reconnect :: IO ()
    reconnect = do
      threadDelay 1000000

      tryAny (Client.connect client) >>= \case
        Left _ ->
          reconnect

        Right () ->
          atomically (writeTVar statusVar Connected)

disconnect :: Client -> IO ()
disconnect client = do
  mask_ $ do
    atomically (writeTVar (statusVar client) Disconnected)
    Client.disconnect (inner client)

waitForConnected :: TVar Status -> IO ()
waitForConnected statusVar = do
  atomically $
    readTVar statusVar >>= \case
      Disconnected -> undefined -- TODO disconnected error
      Connecting -> retry
      Connected -> pure ()

exchange ::
     Client
  -> Message
  -> IO (Maybe Message)
exchange client request =
  loop

  where
    loop :: IO (Maybe Message)
    loop = do
      waitForConnected (statusVar client)

      tryAny (Client.exchange (inner client) request) >>= \case
        Left _ -> do
          atomically (writeTVar (statusVar client) Connecting)
          loop

        Right response ->
          pure response

stream ::
     forall r.
     Client
  -> Message
  -> (IO (Maybe Message) -> IO r)
  -> IO r
stream client request callback =
  loop

  where
    loop :: IO r
    loop = do
      waitForConnected (statusVar client)

      tryAny (Client.stream (inner client) request callback) >>= \case
        Left _ -> do
          atomically (writeTVar (statusVar client) Connecting)
          loop

        Right response ->
          pure response
