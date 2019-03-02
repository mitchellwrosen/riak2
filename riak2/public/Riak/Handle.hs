module Riak.Handle
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , HandleError(..)
  , withHandle
    -- * Re-exports
  , Endpoint(..)
  ) where

import Libriak.Connection (Endpoint(..))
import RiakHandle
