module Riak.Opts
  ( GetOpts(..)
  , PutOpts(..)
  ) where

import Riak.Internal.Prelude
import Riak.Quorum           (Quorum)


data GetOpts
  = GetOpts
  { basicQuorum :: !Bool
  , n :: !Quorum
  , notfoundOk :: !Bool
  , pr :: !Quorum
  , r :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32)
  } deriving stock (Generic, Show)

data PutOpts
  = PutOpts
  { dw :: !Quorum
  , pw :: !Quorum
  , n :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32)
  , w :: !Quorum
  } deriving stock (Generic, Show)
