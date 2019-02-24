module RiakGetOpts where

import RiakReadQuorum (ReadQuorum)

import Data.Default.Class (Default(..))
import Data.Time          (NominalDiffTime)


-- TODO basicQuorum/notfoundOk -> NotfoundBehavior
data GetOpts
  = GetOpts
  { basicQuorum :: !(Maybe Bool)
  , nodes :: !(Maybe Natural)
  , notfoundOk :: !(Maybe Bool)
  , quorum :: !(Maybe ReadQuorum)
  , timeout :: !(Maybe NominalDiffTime)
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { basicQuorum = Nothing
      , nodes = Nothing
      , notfoundOk = Nothing
      , quorum = Nothing
      , timeout = Nothing
      }
