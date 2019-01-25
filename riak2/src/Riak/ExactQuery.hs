module Riak.ExactQuery
  ( ExactQuery(..)
  ) where

import Riak.Internal.Prelude

---- | An exact query on a secondary index.
data ExactQuery
  = Binary !ByteString !ByteString
  | Integer !ByteString !Int64
