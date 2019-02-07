module Riak.Key
  ( -- * Key
    Key(..)
  , generatedKey
  ) where

import Riak.Internal.Bucket  (Bucket(..))
import Riak.Internal.Key     (Key(..))

import qualified ByteString


-- | Use 'generatedKey' to ask Riak to generate a random key when writing a new
-- object or data type.
generatedKey :: Bucket -> Key
generatedKey (Bucket bucketType bucket) =
  Key bucketType bucket ByteString.empty
