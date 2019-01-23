module Riak.Metadata
  ( Metadata(..)
  ) where

import Riak.Internal.Prelude
import Riak.Vtag             (Vtag)


-- | Object metadata.
data Metadata
  = Metadata
  { deleted :: Bool
  , lastModified :: Maybe UTCTime
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  , vtag :: Maybe Vtag
  } deriving stock (Generic, Show)


