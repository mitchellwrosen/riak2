module RiakGetOpts where

import RiakReadQuorum (ReadQuorum)

import Data.Default.Class (Default(..))


-- TODO better names for pr/r
-- TODO basicQuorum/notfoundOk -> NotfoundBehavior
data GetOpts
  = GetOpts
  { basicQuorum :: !(Maybe Bool)
  , nodes :: !(Maybe Natural)
  , notfoundOk :: !(Maybe Bool)
  , quorum :: !(Maybe ReadQuorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
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
