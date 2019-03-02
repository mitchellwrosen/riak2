module Riak.Handle
  ( Handle
  , HandleConfig(..)
  , ReconnectSettings(..)
  , EventHandlers(..)
  , HandleError
  , withHandle
    -- * Re-exports
  , Endpoint(..)
  ) where

import RiakHandle
import Libriak.Connection (Endpoint(..))
