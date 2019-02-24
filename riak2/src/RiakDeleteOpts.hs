module RiakDeleteOpts where

import RiakQuorum      (Quorum)
import RiakReadQuorum  (ReadQuorum)
import RiakWriteQuorum (WriteQuorum)

import Data.Default.Class (Default(..))


data DeleteOpts
  = DeleteOpts
  { nodes :: !(Maybe Quorum)
  , readQuorum :: !(Maybe ReadQuorum)
  , timeout :: !(Maybe Word32)
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


