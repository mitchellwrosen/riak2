module Riak.ConvergentMap
  ( -- * Convergent map operations
    getMap
  , putMap
    -- * Convergent map
  , ConvergentMap
  , newMap
  , mapKey
  , mapValue
    -- * Convergent map value
  , ConvergentMapValue(..)
  , emptyMapValue
  ) where

import RiakMap
import RiakMapValue
