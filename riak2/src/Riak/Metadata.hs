module Riak.Metadata
  ( Metadata(..)
  ) where

import Riak.Internal.Prelude


-- | Object metadata.
data Metadata
  = Metadata
  { deleted :: Bool
  , lastModified :: UTCTime
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  } deriving stock (Generic, Show)


