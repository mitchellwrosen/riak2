-- | Riak connection manager.
--
-- Some details about the implementation:
--
-- * Contains a static array of @Maybe RiakConnection@. This is the maximum
--   number of open sockets.
--
-- * When a thread checks out a connection, its hashed thread id is used to
--   select the connection. If the connection is not open yet, the thread takes
--   a lock, then opens it.
--
-- * If an IO exception is thrown by a thread that has a connection checked out,
--   the connection will be closed (any any threads currently sharing the
--   connection will of course fail as well). The next thread that attempts to
--   use this connection will find it closed, and open it.
--
-- * Inactive sockets are closed by a background thread after some seconds
--   (configurable).

module Riak.Internal.Manager
  ( RiakManager(..)
  , createRiakManager
  , withRiakConnection
  ) where

import Data.Fixed                   (E6, Fixed)
import Data.Hashable                (hash)
import Data.Primitive.MutVar
import Data.Primitive.UnliftedArray
import Data.TimerWheel              (TimerWheel)
import GHC.Prim                     (RealWorld)
import Network.Socket               (HostName, PortNumber)

import qualified Data.TimerWheel as TimerWheel

import Riak.Internal.Connection
import Riak.Internal.Debug
import Riak.Internal.Prelude    hiding (IORef)


type IORef
  = MutVar RealWorld


data Entry
  = NotConnected
  | Connected
      !RiakConnection -- Connection
      !(IORef (IO Bool)) -- Action to cancel the timer that closes the connection


-- | A Riak connection manager.
data RiakManager
  = RiakManager
      !HostName
      !PortNumber
      !(UnliftedArray (IORef Entry))
      !(MVar ()) -- Lock to prevent more than 1 connection from being opened
      !TimerWheel
      !(Fixed E6)


-- | Create a 'RiakManager'.
--
-- In the current implementation, the approximate number of seconds is delayed
-- by a maximum of 25%, and has a minimum of one second, e.g.:
--
-- * A value of __@0.1@__ will be rounded up to __@1@__.
--
-- * A value of __@10@__ will result in a socket that closes between
--   __@10@__ and __@12.5@__ seconds of inactivity.
createRiakManager
  :: HostName -- ^ Host
  -> PortNumber -- ^ Port
  -> Int -- ^ Maximum number of open sockets
  -> Fixed E6 -- ^ Approximate number of seconds after which an inactive socket is closed.
  -> IO RiakManager
createRiakManager host port n inactive = do
  conns :: MutableUnliftedArray RealWorld (IORef Entry) <-
    unsafeNewUnliftedArray n

  for_ [0..n-1] $ \i ->
    writeUnliftedArray conns i =<< newMutVar NotConnected

  conns' :: UnliftedArray (IORef Entry) <-
    unsafeFreezeUnliftedArray conns

  lock :: MVar () <-
    newMVar ()

  -- Attach a finalizer to the lock (since we have it lying around) that closes
  -- all of the open connections.
  (void . mkWeakMVar lock) $
    for_ [0..n-1] $ \i ->
      readMutVar (indexUnliftedArray conns' i) >>= \case
        NotConnected ->
          pure ()

        Connected conn _ ->
          void (tryAny (riakDisconnect conn))

  -- TODO configurable wheel spokes?
  wheel :: TimerWheel <-
    TimerWheel.new 1024 (max 1 (inactive / 4))

  pure (RiakManager host port conns' lock wheel inactive)


withRiakConnection :: RiakManager -> (RiakConnection -> IO a) -> IO a
withRiakConnection (RiakManager host port conns lock wheel inactive) k = do
  which :: Int <-
    (`mod` sizeofUnliftedArray conns) . hash <$> myThreadId

  let
    connVar :: IORef Entry
    connVar =
      indexUnliftedArray conns which

  bracketOnError
    (acquireRiakConnection host port which lock wheel inactive connVar)
    (releaseRiakConnection which connVar)
    k

acquireRiakConnection
  :: HostName
  -> PortNumber
  -> Int
  -> MVar ()
  -> TimerWheel
  -> Fixed E6
  -> IORef Entry
  -> IO RiakConnection
acquireRiakConnection host port which lock wheel inactive connVar =
  readMutVar connVar >>= \case
    NotConnected ->
      withMVar lock $ \_ ->
        readMutVar connVar >>= \case
          NotConnected -> do
            debug ("[riak] manager: opening connection " ++ show which)

            -- Mask asynchronous exceptions because if we actually aquire one,
            -- we definitely want to write it to the array of connections before
            -- returning from this action. Otherwise, we might die from an async
            -- exception and leave the connection open.
            mask $ \unmask -> do
              conn :: RiakConnection <-
                unmask (riakConnect host port)

              cancelRef :: IORef (IO Bool) <-
                newMutVar =<< registerClose conn

              writeMutVar connVar (Connected conn cancelRef)

              pure conn

          Connected conn cancelRef ->
            reregisterClose conn cancelRef

    Connected conn cancelRef ->
      reregisterClose conn cancelRef

 where
  -- Given an open connection, cancel its outstanding close timer, restart it,
  -- and return the connection. If canceling fails, that means the reaper thread
  -- snuck in underneath us and closed the connection; just re-acquire the
  -- connection.
  reregisterClose :: RiakConnection -> IORef (IO Bool) -> IO RiakConnection
  reregisterClose conn cancelRef = do
    canceled :: Bool <-
      join (readMutVar cancelRef)

    if canceled
      then do
        writeMutVar cancelRef =<< registerClose conn
        pure conn
      else do
        acquireRiakConnection host port which lock wheel inactive connVar

  -- Register a timer that closes this connection when it fires.
  registerClose :: RiakConnection -> IO (IO Bool)
  registerClose conn = do
    TimerWheel.register
      inactive
      (do
        debug $
          "[riak] manager: closing connection " ++ show which ++ " after " ++
            show inactive ++ " seconds of inactivity"

        writeMutVar connVar NotConnected
        void (tryAny (riakDisconnect conn)))
      wheel

releaseRiakConnection
  :: Int
  -> IORef Entry
  -> RiakConnection
  -> IO ()
releaseRiakConnection which connVar conn = do
  writeMutVar connVar NotConnected
  riakDisconnect conn
  debug ("[riak] manager: exception on connection " ++ show which)
