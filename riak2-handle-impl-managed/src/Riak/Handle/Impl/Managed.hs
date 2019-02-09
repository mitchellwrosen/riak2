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
  , Error(..)
  ) where

import Riak.Request  (Request)
import Riak.Response (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (bracket)
import Control.Monad          (forever, void)
import Foreign.C              (CInt)


newtype Handle
  = Handle
  { handleVar :: TMVar Handle.Handle
  }

type Config
  = Handle.Config

data Error
  = Error
  deriving stock (Eq, Show)

-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
withHandle ::
     Config
  -> (Handle -> IO a)
  -> IO (Either CInt a)
withHandle config k = do
  handleVar :: TMVar Handle.Handle <-
    newEmptyTMVarIO

  bracket
    (forkIOWithUnmask $ \unmask ->
      unmask (manager config handleVar))
    killThread
    (\_ ->
      Right <$>
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
    loop >>= \case
      Left errno ->
        print errno

      Right () ->
        pure ()

  where
    loop :: IO (Either CInt ())
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
  -> IO (Either Error Response)
exchange Handle { handleVar } request =
  loop

  where
    loop :: IO (Either Error Response)
    loop = do
      handle :: Handle.Handle <-
        atomically (readTMVar handleVar)

      Handle.exchange handle request >>= \case
        Left _ -> do
          void (atomically (takeTMVar handleVar))
          loop

        Right response ->
          pure (Right response)

-- | Send a request and stream the response (one or more messages).
stream ::
     -- forall r.
     -- Handle
  -- -> Request
  -- -> (IO (Either Error Response) -> IO r)
  -- -> IO (Either Error r)
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either Error r)
stream Handle { handleVar } request value step =
  loop

  where
    loop :: IO (Either Error r)
    loop = do
      handle :: Handle.Handle <-
        atomically (readTMVar handleVar)

      Handle.stream handle request value step >>= \case
        Left _ -> do
          void (atomically (takeTMVar handleVar))
          loop

        Right response ->
          pure (Right response)
