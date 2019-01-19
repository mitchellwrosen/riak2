module Riak.Opts
  ( GetObjectOpts(..)
  ) where

import Riak.Internal.Prelude
import Riak.Quorum (Quorum)


data GetObjectOpts
  = GetObjectOpts
  { basicQuorum :: !Bool
  , head :: !Bool
  , n :: !Quorum
  , notfoundOk :: !Bool
  , pr :: !Quorum
  , r :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32)
  } deriving stock (Generic, Show)
