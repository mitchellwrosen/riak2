-- | Riak connection manager.
--
-- Some details about the implementation:
--
-- * Contains a static array of one @Maybe RiakConnection@ per capability. This
--   is the maximum number of open sockets.
--
-- * When a thread checks out a connection, its hashed thread id is used to
--   select the connection. If the connection is not open yet, open it.
--
-- * If an IO exception is thrown by a thread that has a connection checked out,
--   the connection will be closed (any any threads currently sharing the
--   connection will of course fail as well). The next thread that attempts to
--   use this connection will find it closed, and open it.
--
-- * Currently, inactive sockets are never closed. TODO close inactive sockets.

module Riak.Internal.Manager
  ( RiakManager(..)
  , createRiakManager
  , withRiakConnection
  ) where

import Data.Hashable                (hash)
import Data.Primitive.MutVar
import Data.Primitive.UnliftedArray
import GHC.Prim                     (RealWorld)
import Network.Socket               (HostName, PortNumber)

import Riak.Internal.Connection
import Riak.Internal.Debug
import Riak.Internal.Prelude


-- | A Riak connection manager.
data RiakManager
  = RiakManager
      !HostName
      !PortNumber
      !(UnliftedArray (MutVar RealWorld (Maybe RiakConnection)))
      !(MVar ()) -- Lock to prevent more than 1 connection from being opened


createRiakManager :: HostName -> PortNumber -> IO RiakManager
createRiakManager host port = do
  caps :: Int <-
    getNumCapabilities

  conns :: MutableUnliftedArray RealWorld
            (MutVar RealWorld (Maybe RiakConnection)) <-
    unsafeNewUnliftedArray caps

  for_ [0..caps-1] $ \i ->
    writeUnliftedArray conns i =<< newMutVar Nothing

  conns' :: UnliftedArray (MutVar RealWorld (Maybe RiakConnection)) <-
    unsafeFreezeUnliftedArray conns

  lock :: MVar () <-
    newMVar ()

  pure (RiakManager host port conns' lock)


withRiakConnection :: RiakManager -> (RiakConnection -> IO a) -> IO a
withRiakConnection (RiakManager host port conns lock) k = do
  which :: Int <-
    (`mod` sizeofUnliftedArray conns) . hash <$> myThreadId

  let
    connVar :: MutVar RealWorld (Maybe RiakConnection)
    connVar =
      indexUnliftedArray conns which

  let
    acquire :: IO RiakConnection
    acquire =
      readMutVar connVar >>= \case
        Nothing ->
          withMVar lock $ \_ ->
            readMutVar connVar >>= \case
              Nothing -> do
                debug ("[riak] manager: opening connection " ++ show which)
                conn <- riakConnect host port
                writeMutVar connVar (Just conn)
                pure conn
              Just conn ->
                pure conn
        Just conn ->
          pure conn

  let
    release :: RiakConnection -> IO ()
    release conn = do
      debug ("[riak] manager: exception on connection " ++ show which)
      writeMutVar connVar Nothing
      riakDisconnect conn

  bracketOnError acquire release k
