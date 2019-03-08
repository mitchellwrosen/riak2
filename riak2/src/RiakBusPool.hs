-- | A pool of managed buses.

module RiakBusPool
  ( BusPool
  , createBusPool
  , withManagedBus
  ) where

import Libriak.Connection (Endpoint)
import RiakManagedBus     (EventHandlers, ManagedBus, ManagedBusConfig(..),
                           createManagedBus)

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
      (\uuid -> createManagedBus (makeManagedBusConfig uuid))

  pure BusPool
    { pool = pool }

  where
    makeManagedBusConfig :: Int -> ManagedBusConfig
    makeManagedBusConfig uuid =
      ManagedBusConfig
        { uuid = uuid
        , endpoint = endpoint
        , healthCheckInterval = healthCheckInterval
        , idleTimeout = idleTimeout
        , receiveTimeout = receiveTimeout
        , handlers = handlers
        }

withManagedBus ::
     BusPool
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus BusPool { pool } callback = do
  threadId :: ThreadId <-
    myThreadId

  callback (pool ! (hash threadId `mod` Vector.length pool))
