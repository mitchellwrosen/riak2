-- | A Riak handle that contains an array of Riak handles.
--
-- TODO striped handle configuration

module Riak.Handle.Impl.Striped
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
import Control.Monad              (join, replicateM)
import Data.Hashable (hash)
import Control.Monad.Codensity
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except
import Data.Vector                (Vector)

import qualified Data.Vector as Vector


data Handle
  = Handle !(Vector Handle.Handle)

type HandleConfig
  = Handle.HandleConfig

type HandleError
  = Handle.HandleError

type HandleConnectError
  = Handle.HandleConnectError


-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either HandleConnectError a)
withHandle config onSuccess =
  runExceptT (withHandle_ config (lift . onSuccess))

withHandle_ ::
     HandleConfig
  -> (Handle -> ExceptT HandleConnectError IO a)
  -> ExceptT HandleConnectError IO a
withHandle_ config onSuccess =
  runCodensity
    (replicateM 10 (acquireHandle config))
    (withHandles onSuccess)

withHandles ::
     (Handle -> ExceptT HandleConnectError IO a)
  -> [Handle.Handle]
  -> ExceptT HandleConnectError IO a
withHandles onSuccess handles =
  onSuccess (Handle (Vector.fromList handles))

acquireHandle ::
     HandleConfig
  -> Codensity (ExceptT HandleConnectError IO) Handle.Handle
acquireHandle config =
  Codensity acquire

  where
    acquire ::
         (Handle.Handle -> ExceptT HandleConnectError IO a)
      -> ExceptT HandleConnectError IO a
    acquire k =
      ExceptT
        (join <$>
          Handle.withHandle config (\handle -> runExceptT (k handle)))

-- | Send a request and receive the response (a single message).
exchange ::
     Handle
  -> Request
  -> IO (Either HandleError Response)
exchange handles request = do
  handle <- randomHandle handles
  Handle.exchange handle request

-- | Send a request and stream the response (one or more messages).
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either HandleError r)
stream handles request value step = do
  handle <- randomHandle handles
  Handle.stream handle request value step

-- | Acquire a random handle by hashing the current thread id.
randomHandle :: Handle -> IO Handle.Handle
randomHandle (Handle handles) = do
  thread <- myThreadId
  pure (handles Vector.! (hash thread `rem` Vector.length handles))
