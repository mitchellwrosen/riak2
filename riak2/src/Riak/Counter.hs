module Riak.Counter where

import Riak.Internal.Prelude
import Riak.Key (Key)


data Counter
  = Counter
  { key :: !Key
  , value :: !Int64
  }
