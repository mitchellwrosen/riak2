-- | A pool of managed buses.

module RiakBusPool
  ( BusPool
  , createBusPool
  , withManagedBus
  ) where

import RiakManagedBus (EventHandlers, ManagedBus, ManagedBusConfig(..),
                       createManagedBus)

import Data.Hashable      (hash)
import Data.Vector        (Vector, (!))
import Socket.Stream.IPv4 (Endpoint)

import qualified Data.Vector as Vector


-- TODO pool finalizer
data BusPool
  = BusPool
  { pool :: Vector ManagedBus
  }

-- TODO bus pool config type

-- | Create a bus pool.
--
-- /Throws/. This function will never throw an exception.
createBusPool ::
     Endpoint
  -> Int -- ^ Health check interval (microseconds)
  -> Int -- ^ Idle timeout (microseconds)
  -> Int -- ^ Request timeout (microseconds)
  -> Int -- ^ Connect timeout (microseconds)
  -> EventHandlers
  -> IO BusPool
createBusPool
    endpoint healthCheckInterval idleTimeout requestTimeout connectTimeout
    handlers = do

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
        , requestTimeout = requestTimeout
        , connectTimeout = connectTimeout
        , handlers = handlers
        }

-- TODO allow different ways to select a connection than thread id
--
-- Some numbers:
--
-- hash thread id    5ns
-- current time     20ns
-- mwc-random       20ns
-- random          185ns
withManagedBus ::
     BusPool
  -> (ManagedBus -> IO a)
  -> IO a
withManagedBus BusPool { pool } callback = do
  threadId :: ThreadId <-
    myThreadId

  callback (pool ! (hash threadId `mod` Vector.length pool))
