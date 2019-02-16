module Riak.ConvergentMap
  ( -- * Convergent map operations
    getConvergentMap
  , putConvergentMap
    -- * Convergent map
  , ConvergentMap
  , newConvergentMap
  , convergentMapKey
  , convergentMapValue
    -- * Convergent map value
  , ConvergentMapValue(..)
  , emptyConvergentMapValue
  ) where

import Riak.Internal.ConvergentMap
