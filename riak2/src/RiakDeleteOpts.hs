module RiakDeleteOpts where

import RiakQuorum (Quorum)
import RiakReadQuorum (ReadQuorum)

import Data.Default.Class (Default(..))


data DeleteOpts
  = DeleteOpts
  { dw :: !(Maybe Quorum)
  , nodes :: !(Maybe Quorum)
  , pw :: !(Maybe Quorum)
  , readQuorum :: !(Maybe ReadQuorum)
  , timeout :: !(Maybe Word32)
  , w :: !(Maybe Quorum)
  } deriving stock (Generic, Show)

instance Default DeleteOpts where
  def :: DeleteOpts
  def =
    DeleteOpts
      { dw = Nothing
      , nodes = Nothing
      , pw = Nothing
      , readQuorum = Nothing
      , timeout = Nothing
      , w = Nothing
      }


