module Riak.Metadata
  ( Metadata(..)
  ) where

import Riak.Internal.Prelude


-- | Object metadata provided when reading an object, but not required when
-- writing.
data Metadata
  = Metadata
  { deleted :: Bool
  , lastModified :: UTCTime
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  } deriving stock (Generic, Show)


