-- | A pool of buses.

module RiakBusPool
  ( BusPool
  , createBusPool
  , withBus
  ) where

import RiakBus (Bus, BusConfig(..), EventHandlers, createBus)

import Data.Hashable      (hash)
import Data.Vector        (Vector, (!))
import Socket.Stream.IPv4 (Endpoint)

import qualified Data.Vector as Vector


-- TODO pool finalizer
data BusPool
  = BusPool
  { pool :: Vector Bus
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

  pool :: Vector Bus <-
    Vector.generateM
      256 -- TODO configure bus pool size
      (\uuid -> createBus (makeBusConfig uuid))

  pure BusPool
    { pool = pool }

  where
    makeBusConfig :: Int -> BusConfig
    makeBusConfig uuid =
      BusConfig
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
withBus ::
     BusPool
  -> (Bus -> IO a)
  -> IO a
withBus BusPool { pool } callback = do
  threadId :: ThreadId <-
    myThreadId

  callback (pool ! (hash threadId `mod` Vector.length pool))
