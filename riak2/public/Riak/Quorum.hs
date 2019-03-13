module Riak.Quorum
  ( Quorum(..)
  , ReadQuorum(..)
  , NotfoundOk(..)
  , WriteQuorum(..)
  ) where

import RiakQuorum      (Quorum(..))
import RiakReadQuorum  (NotfoundOk(..), ReadQuorum(..))
import RiakWriteQuorum (WriteQuorum(..))
