module RiakGetOpts where

import RiakReadQuorum (ReadQuorum)

import Data.Default.Class (Default(..))
import Data.Time          (NominalDiffTime)


data GetOpts
  = GetOpts
  { nodes :: Maybe Natural
  , quorum :: Maybe ReadQuorum
  , timeout :: Maybe NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { nodes = Nothing
      , quorum = Nothing
      , timeout = Nothing
      }
