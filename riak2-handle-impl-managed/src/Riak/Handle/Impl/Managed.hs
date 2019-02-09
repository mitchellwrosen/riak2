-- | A Riak client that manages another client by reconnecting automatically.
--
-- Still TODO: some way of configuring when we want to reconnect, and when we
-- want to give up on the connection permanently.

module Riak.Handle.Impl.Managed
  ( Handle
  , Config
  , withHandle
  , exchange
  , stream
  , Exception
  , isRemoteShutdownException
  ) where

import Riak.Request  (Request)
import Riak.Response (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, catchAny, tryAny)
import Control.Monad          (forever, void)

import qualified Control.Exception as Exception


newtype Handle
  = Handle
  { handleVar :: TMVar Handle.Handle
  }

type Config
  = Handle.Config

withHandle ::
     Config
  -> (Handle -> IO a)
  -> IO a
withHandle config k = do
  handleVar :: TMVar Handle.Handle <-
    newEmptyTMVarIO

  bracket
    (forkIOWithUnmask $ \unmask ->
      unmask (manager config handleVar))
    killThread
    (\_ ->
      k Handle
        { handleVar = handleVar
        })

-- The manager thread:
--
-- * Acquire an underlying connection.
-- * Smuggle it out to the rest of the world via a TMVar.
-- * Wait for this TMVar to empty out, then repeat.
--
-- Meanwhile, users of this handle (via exchange/stream) grab the underlying
-- handle (if available), use it, and if anything goes wrong, empty out the
-- TMVar and retry.
manager :: Config -> TMVar Handle.Handle -> IO ()
manager config handleVar =
  -- TODO how to handle manager thread crashing?
  forever $
    loop `catchAny` \e ->
      print e

  where
    loop :: IO ()
    loop =
      Handle.withHandle config $ \handle -> do
        atomically (putTMVar handleVar handle)
        atomically $
          isEmptyTMVar handleVar >>= \case
            True -> pure ()
            False -> retry

-- | Send a request and receive the response (a single message).
exchange ::
     Handle
  -> Request
  -> IO Response
exchange Handle { handleVar } request =
  loop

  where
    loop :: IO Response
    loop = do
      handle :: Handle.Handle <-
        atomically (readTMVar handleVar)

      tryAny (Handle.exchange handle request) >>= \case
        Left _ -> do
          void (atomically (takeTMVar handleVar))
          loop

        Right response ->
          pure response

-- | Send a request and stream the response (one or more messages).
stream ::
     forall r.
     Handle
  -> Request
  -> (IO Response -> IO r)
  -> IO r
stream Handle { handleVar } request callback =
  loop

  where
    loop :: IO r
    loop = do
      handle :: Handle.Handle <-
        atomically (readTMVar handleVar)

      tryAny (Handle.stream handle request callback) >>= \case
        Left _ -> do
          void (atomically (takeTMVar handleVar))
          loop

        Right response ->
          pure response


data Exception
  deriving stock (Show)
  deriving anyclass (Exception.Exception)

isRemoteShutdownException :: Exception -> Bool
isRemoteShutdownException _ =
  False
