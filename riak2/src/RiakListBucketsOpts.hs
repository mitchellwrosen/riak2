module RiakListBucketsOpts where

import Data.Default.Class (Default(..))
import Data.Time          (NominalDiffTime)


data ListBucketsOpts
  = ListBucketsOpts
  { timeout :: Maybe NominalDiffTime
  } deriving stock (Eq, Generic, Show)

instance Default ListBucketsOpts where
  def =
    ListBucketsOpts
      { timeout = Nothing
      }
