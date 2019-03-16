module RiakListKeysOpts where

import Data.Default.Class (Default(..))
import Data.Time          (NominalDiffTime)


data ListKeysOpts
  = ListKeysOpts
  { timeout :: Maybe NominalDiffTime
  } deriving stock (Eq, Generic, Show)

instance Default ListKeysOpts where
  def =
    ListKeysOpts
      { timeout = Nothing
      }
