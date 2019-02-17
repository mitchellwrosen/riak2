module RiakConvergentMap
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

import RiakInternalConvergentMap
import RiakInternalConvergentMapValue
