module Riak.Opts
  ( GetOpts(..)
  , PutOpts(..)
  ) where

import Riak.Internal.Prelude
import Riak.Quorum           (Quorum)

import Data.Default.Class (Default(..))


data GetOpts
  = GetOpts
  { basicQuorum :: !Bool
  , n :: !(Maybe Quorum)
  , notfoundOk :: !(Maybe Bool)
  , pr :: !(Maybe Quorum)
  , r :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { basicQuorum = False
      , n = Nothing
      , notfoundOk = Nothing
      , pr = Nothing
      , r = Nothing
      , timeout = Nothing
      }

data PutOpts
  = PutOpts
  { dw :: !(Maybe Quorum)
  , pw :: !(Maybe Quorum)
  , n :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  , w :: !(Maybe Quorum)
  } deriving stock (Generic, Show)

instance Default PutOpts where
  def :: PutOpts
  def =
    PutOpts
      { dw = Nothing
      , pw = Nothing
      , n = Nothing
      , timeout = Nothing
      , w = Nothing
      }
