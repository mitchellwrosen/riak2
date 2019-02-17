-- | A Riak handle that contains an array of Riak handles.
--
-- TODO striped handle configuration

module Riak.Handle.Impl.Striped
  ( Handle
  , HandleConfig
  , withHandle
  , exchange
  , stream
    -- ** Re-exports
  , ConnectError(..)
  , ConnectionError(..)
  ) where

import Libriak.Connection (ConnectError(..), ConnectionError(..))
import Libriak.Request    (Request)
import Libriak.Response   (Response)

import qualified Riak.Handle.Signature as Handle

import Control.Concurrent
import Control.Monad              (join, replicateM)
import Control.Monad.Codensity
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except
import Data.Hashable              (hash)
import Data.Vector                (Vector)

import qualified Data.Vector as Vector


data Handle
  = Handle !(Vector Handle.Handle)

type HandleConfig
  = Handle.HandleConfig


-- | Acquire a handle.
--
-- /Throws/. Whatever the underlying handle might throw during its 'withHandle'.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO (Either ConnectError a)
withHandle config onSuccess =
  runExceptT (withHandle_ config (lift . onSuccess))

withHandle_ ::
     HandleConfig
  -> (Handle -> ExceptT ConnectError IO a)
  -> ExceptT ConnectError IO a
withHandle_ config onSuccess =
  runCodensity
    (replicateM 10 (acquireHandle config))
    (withHandles onSuccess)

withHandles ::
     (Handle -> ExceptT ConnectError IO a)
  -> [Handle.Handle]
  -> ExceptT ConnectError IO a
withHandles onSuccess handles =
  onSuccess (Handle (Vector.fromList handles))

acquireHandle ::
     HandleConfig
  -> Codensity (ExceptT ConnectError IO) Handle.Handle
acquireHandle config =
  Codensity acquire

  where
    acquire ::
         (Handle.Handle -> ExceptT ConnectError IO a)
      -> ExceptT ConnectError IO a
    acquire k =
      ExceptT
        (join <$>
          Handle.withHandle config (\handle -> runExceptT (k handle)))

-- | Send a request and receive the response (a single message).
exchange ::
     Handle
  -> Request
  -> IO (Either ConnectionError Response)
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
  -> IO (Either ConnectionError r)
stream handles request value step = do
  handle <- randomHandle handles
  Handle.stream handle request value step

-- | Acquire a random handle by hashing the current thread id.
randomHandle :: Handle -> IO Handle.Handle
randomHandle (Handle handles) = do
  thread <- myThreadId
  pure (handles Vector.! (hash thread `rem` Vector.length handles))
