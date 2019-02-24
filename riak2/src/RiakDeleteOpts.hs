module RiakDeleteOpts where

import RiakReadQuorum  (ReadQuorum)
import RiakWriteQuorum (WriteQuorum)

import Data.Default.Class (Default(..))
import Data.Time          (NominalDiffTime)


data DeleteOpts
  = DeleteOpts
  { nodes :: !(Maybe Natural)
  , readQuorum :: !(Maybe ReadQuorum)
  , timeout :: !(Maybe NominalDiffTime)
  , writeQuorum :: !(Maybe WriteQuorum)
  } deriving stock (Generic, Show)

instance Default DeleteOpts where
  def :: DeleteOpts
  def =
    DeleteOpts
      { nodes = Nothing
      , readQuorum = Nothing
      , timeout = Nothing
      , writeQuorum = Nothing
      }


