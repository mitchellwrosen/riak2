-- | A pool of managed buses.

module RiakBusPool
  ( BusPool
  , createBusPool
  , withManagedBus
  ) where

import Libriak.Connection (Endpoint)
import RiakManagedBus     (EventHandlers, ManagedBus, createManagedBus)

import Data.Hashable (hash)
import Data.Vector   (Vector, (!))

import qualified Data.Vector as Vector


-- TODO pool finalizer
data BusPool
  = BusPool
  { pool :: !(Vector ManagedBus)
  }

-- | Create a bus pool.
--
-- /Throws/. This function will never throw an exception.
createBusPool ::
     Endpoint
  -> Int -- ^ Health check interval (microseconds)
  -> Int -- ^ Idle timeout (microseconds)
  -> Int -- ^ Receive timeout (microseconds)
  -> EventHandlers
  -> IO BusPool
createBusPool
    endpoint healthCheckInterval idleTimeout receiveTimeout handlers = do

  pool :: Vector ManagedBus <-
    Vector.generateM
      256 -- TODO configure bus pool size
      (\i ->
        createManagedBus
          i
          endpoint
          healthCheckInterval
          idleTimeout
          receiveTimeout
          handlers)

  pure BusPool
    { pool = pool }

withManagedBus ::
     BusPool
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus BusPool { pool } callback = do
  threadId :: ThreadId <-
    myThreadId

  callback (pool ! (hash threadId `mod` Vector.length pool))
