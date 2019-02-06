-- | A Riak client that manages another client by reconnecting automatically.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Interface.Impl.Managed
  ( Interface
  , Config
  , withInterface
  , exchange
  , stream
  , Exception
  , isRemoteShutdownException
  ) where

import Riak.Request  (Request)
import Riak.Response (Response)

import qualified Riak.Interface.Signature as Interface

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad          (forever, void)
import UnliftIO.Exception     (bracket, tryAny)

import qualified Control.Exception as Exception


newtype Interface
  = Interface
  { ifaceVar :: TMVar Interface.Interface
  }

type Config
  = Interface.Config

withInterface ::
     Config
  -> (Interface -> IO a)
  -> IO a
withInterface config k = do
  ifaceVar :: TMVar Interface.Interface <-
    newEmptyTMVarIO

  bracket
    (forkIOWithUnmask $ \unmask ->
      unmask (manager config ifaceVar))
    killThread
    (\_ ->
      k Interface
        { ifaceVar = ifaceVar
        })

-- The manager thread:
--
-- * Acquire an underlying connection.
-- * Smuggle it out to the rest of the world via a TMVar.
-- * Wait for this TMVar to empty out, then repeat.
--
-- Meanwhile, users of this interface (via exchange/stream) grab the underlying
-- interface (if available), use it, and if anything goes wrong, empty out the
-- TMVar and retry.
manager :: Config -> TMVar Interface.Interface -> IO ()
manager config ifaceVar =
  forever loop

  where
    loop :: IO ()
    loop =
      Interface.withInterface config $ \iface -> do
        atomically (putTMVar ifaceVar iface)
        atomically $
          isEmptyTMVar ifaceVar >>= \case
            True -> pure ()
            False -> retry

-- | Send a request and receive the response (a single message).
exchange ::
     Interface
  -> Request
  -> IO Response
exchange Interface { ifaceVar } request =
  loop

  where
    loop :: IO Response
    loop = do
      iface :: Interface.Interface <-
        atomically (readTMVar ifaceVar)

      tryAny (Interface.exchange iface request) >>= \case
        Left _ -> do
          void (atomically (takeTMVar ifaceVar))
          loop

        Right response ->
          pure response

-- | Send a request and stream the response (one or more messages).
stream ::
     forall r.
     Interface
  -> Request
  -> (IO Response -> IO r)
  -> IO r
stream Interface { ifaceVar } request callback =
  loop

  where
    loop :: IO r
    loop = do
      iface :: Interface.Interface <-
        atomically (readTMVar ifaceVar)

      tryAny (Interface.stream iface request callback) >>= \case
        Left _ -> do
          void (atomically (takeTMVar ifaceVar))
          loop

        Right response ->
          pure response


data Exception
  deriving stock (Show)
  deriving anyclass (Exception.Exception)

isRemoteShutdownException :: Exception -> Bool
isRemoteShutdownException _ =
  False
