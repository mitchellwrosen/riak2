-- | A convergent set data type.
--
-- To create a set for the first time, use 'newSet'. From then on, you must
-- follow the get-modify-put update cycle:
--
-- 1. Get the set with 'getSet'
-- 2. Modify the set using the 'setValue' lens
-- 3. Put the modified set using 'putSet'

module Riak.ConvergentSet
  ( -- * Convergent set
    ConvergentSet
  , newSet
  , setKey
  , setValue
    -- ** Operations
  , getSet
  , putSet
    -- *** *With variants
  , getSetWith
  , putSetWith
  ) where

import RiakSet
