module Riak.Handle
  ( Handle
  , HandleConfig(..)
  , EventHandlers(..)
  , HandleError(..)
  , createHandle
    -- * Re-exports
  , Endpoint(..)
  ) where

import Libriak.Connection (Endpoint(..))
import RiakHandle
