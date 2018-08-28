-- | Riak connection manager.
--
-- Some details about the implementation:
--
-- * Contains a static array of @Maybe RiakConnection@, whose length is given
--   during manager creation. This is the maximum number of open sockets.
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


{-# LANGUAGE LambdaCase, NoImplicitPrelude, ScopedTypeVariables #-}

module Riak.Internal.Manager
  ( RiakManager
  , createRiakManager
  , withRiakConnection
  ) where

import Data.Hashable                (hash)
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
      !(UnliftedArray (TVar (Maybe RiakConnection)))
      !(MVar ()) -- Lock to prevent more than 1 connection from being opened


createRiakManager :: HostName -> PortNumber -> Int -> IO RiakManager
createRiakManager host port n = do
  conns :: MutableUnliftedArray RealWorld (TVar (Maybe RiakConnection)) <-
    unsafeNewUnliftedArray n

  for_ [0..n-1] $ \i ->
    writeUnliftedArray conns i =<< newTVarIO Nothing

  conns' :: UnliftedArray (TVar (Maybe RiakConnection)) <-
    unsafeFreezeUnliftedArray conns

  lock :: MVar () <-
    newMVar ()

  pure (RiakManager host port conns' lock)


withRiakConnection :: RiakManager -> (RiakConnection -> IO a) -> IO a
withRiakConnection (RiakManager host port conns lock) k = do
  which :: Int <-
    (`mod` sizeofUnliftedArray conns) . hash <$> myThreadId

  let
    connVar :: TVar (Maybe RiakConnection)
    connVar =
      indexUnliftedArray conns which

  let
    acquire :: IO RiakConnection
    acquire =
      readTVarIO connVar >>= \case
        Nothing ->
          withMVar lock $ \_ ->
            readTVarIO connVar >>= \case
              Nothing -> do
                debug ("[riak] manager: opening connection " ++ show which)
                conn <- riakConnect host port
                atomically (writeTVar connVar (Just conn))
                pure conn
              Just conn ->
                pure conn
        Just conn ->
          pure conn

  let
    release :: RiakConnection -> IO ()
    release conn = do
      debug ("[riak] manager: exception on connection " ++ show which)
      atomically (writeTVar connVar Nothing)
      riakDisconnect conn

  bracketOnError acquire release k
