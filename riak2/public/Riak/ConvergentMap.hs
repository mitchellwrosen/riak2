-- | A convergent map data type.
--
-- To create a map for the first time, use 'newMap'. From then on, you must
-- follow the get-modify-put update cycle:
--
-- 1. Get the map with 'getMap'
-- 2. Modify the map using the 'mapValue' lens
-- 3. Put the modified map using 'putMap'

module Riak.ConvergentMap
  ( -- * Convergent map
    ConvergentMap
  , newMap
  , mapKey
  , mapValue
    -- ** Value
  , ConvergentMapValue(..)
  , emptyMapValue
    -- * Operations
  , getMap
  , putMap
  ) where

import RiakMap
import RiakMapValue
