module RiakPutOpts where

import RiakWriteQuorum (WriteQuorum)

import Data.Default.Class (Default(..))


data PutOpts
  = PutOpts
  { nodes :: !(Maybe Natural)
  , quorum :: !(Maybe WriteQuorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  } deriving stock (Generic, Show)

instance Default PutOpts where
  def :: PutOpts
  def =
    PutOpts
      { nodes = Nothing
      , quorum = Nothing
      , timeout = Nothing
      }

