module Riak.Key
  ( -- * Key
    Key(..)
  , none
  ) where

import Riak.Internal.Key     (Key(..))
import Riak.Internal.Prelude

import qualified Data.ByteString as ByteString


-- | Use 'none' to ask Riak to generate a random key when writing a new object
-- or data type:
--
-- @
-- Key
--   { type' = ...
--   , bucket = ...
--   , key = Riak.Key.none
--   }
-- @
none :: ByteString
none =
  ByteString.empty
