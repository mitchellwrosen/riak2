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

module Riak.Interface.Impl.Managed
  ( Interface
  , new
  , connect
  , disconnect
  , exchange
  , stream
  ) where

import Riak.Message (Message)

import qualified Riak.Interface.Signature as Inner

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad          (void, when)
import System.IO.Unsafe       (unsafeInterleaveIO)
import UnliftIO.Exception


data Interface
  = Interface
  { inner :: !Inner.Interface
  , statusVar :: !(TVar Status)
  , backgroundThreadVar :: !(MVar ThreadId)
  }

data Status
  = Disconnected
  | Connecting
  | Connected

new ::
     Inner.Interface
  -> IO Interface
new inner =
  Interface
    <$> pure inner
    <*> newTVarIO Disconnected
    <*> newEmptyMVar

connect :: Interface -> IO ()
connect =
  spawnBackgroundThreadIfNotRunning

spawnBackgroundThreadIfNotRunning :: Interface -> IO ()
spawnBackgroundThreadIfNotRunning iface = do
  -- TODO background thread lifetime/exception handling
  backgroundThread :: ThreadId <-
    unsafeInterleaveIO
      (forkIO
        (runBackgroundThread (inner iface) (statusVar iface)))

  success :: Bool <-
    tryPutMVar
      (backgroundThreadVar iface)
      backgroundThread

  when success (void (evaluate backgroundThread))

runBackgroundThread :: Inner.Interface -> TVar Status -> IO ()
runBackgroundThread iface statusVar =
  loop

  where
    loop :: IO ()
    loop = do
      continue :: Bool <-
        waitForConnecting

      when continue $ do
        Inner.disconnect iface `catchAny` \_ -> pure ()
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

      tryAny (Inner.connect iface) >>= \case
        Left _ ->
          reconnect

        Right () ->
          atomically (writeTVar statusVar Connected)

disconnect :: Interface -> IO ()
disconnect iface = do
  mask_ $ do
    atomically (writeTVar (statusVar iface) Disconnected)
    Inner.disconnect (inner iface)

waitForConnected :: TVar Status -> IO ()
waitForConnected statusVar = do
  atomically $
    readTVar statusVar >>= \case
      Disconnected -> undefined -- TODO disconnected error
      Connecting -> retry
      Connected -> pure ()

exchange ::
     Interface
  -> Message
  -> IO (Maybe Message)
exchange iface request =
  loop

  where
    loop :: IO (Maybe Message)
    loop = do
      waitForConnected (statusVar iface)

      tryAny (Inner.exchange (inner iface) request) >>= \case
        Left _ -> do
          atomically (writeTVar (statusVar iface) Connecting)
          loop

        Right response ->
          pure response

stream ::
     forall r.
     Interface
  -> Message
  -> (IO (Maybe Message) -> IO r)
  -> IO r
stream iface request callback =
  loop

  where
    loop :: IO r
    loop = do
      waitForConnected (statusVar iface)

      tryAny (Inner.stream (inner iface) request callback) >>= \case
        Left _ -> do
          atomically (writeTVar (statusVar iface) Connecting)
          loop

        Right response ->
          pure response
