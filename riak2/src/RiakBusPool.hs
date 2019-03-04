-- | A pool of managed buses.

module RiakBusPool
  ( BusPool
  , withBusPool
  , withManagedBus
  ) where

import Libriak.Connection (Endpoint)
import RiakManagedBus     (EventHandlers, ManagedBus)

import qualified RiakManagedBus as ManagedBus

import Control.Monad (ap)
import Data.Hashable (hash)
import Data.Vector   (Vector, (!))

import qualified Data.Vector as Vector


data BusPool
  = BusPool
  { pool :: !(Vector ManagedBus)
  }

newtype Managed a
  = Managed { runManaged :: forall r. (a -> IO r) -> IO r }
  deriving stock (Functor)

instance Applicative Managed where
  pure x =
    Managed (\k -> k x)

  (<*>) =
    ap

instance Monad Managed where
  return =
    pure

  Managed mx >>= f =
    Managed (\k -> mx (\x -> runManaged (f x) k))

-- | Acquire a bus pool.
--
-- /Throws/. This function will never throw an exception.
withBusPool ::
     Endpoint
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> (BusPool -> IO a)
  -> IO a
withBusPool endpoint receiveTimeout handlers callback =
  runManaged
    (Vector.replicateM
      256 -- TODO configure bus pool size
      (Managed (ManagedBus.withManagedBus endpoint receiveTimeout handlers)))
    (\buses -> callback (BusPool buses))

withManagedBus ::
     BusPool
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus BusPool { pool } callback = do
  threadId :: ThreadId <-
    myThreadId

  callback (pool ! (hash threadId `mod` Vector.length pool))
