module Riak.Opts
  ( GetOpts(..)
  , PutOpts(..)
  ) where

import Riak.Internal.Prelude
import Riak.Quorum           (Quorum)

import qualified Riak.Quorum as Quorum


data GetOpts
  = GetOpts
  { basicQuorum :: !Bool
  , n :: !Quorum
  , notfoundOk :: !Bool
  , pr :: !Quorum
  , r :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { basicQuorum = False
      , n = Quorum.Default
      , notfoundOk = True
      , pr = Quorum.Default
      , r = Quorum.Default
      , sloppyQuorum = True
      , timeout = Nothing
      }

data PutOpts
  = PutOpts
  { dw :: !Quorum
  , pw :: !Quorum
  , n :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  , w :: !Quorum
  } deriving stock (Generic, Show)

instance Default PutOpts where
  def :: PutOpts
  def =
    PutOpts
      { dw = Quorum.Default
      , pw = Quorum.Default
      , n = Quorum.Default
      , sloppyQuorum = True
      , timeout = Nothing
      , w = Quorum.Default
      }
