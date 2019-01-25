module Riak.Internal.ExactQuery where

import Riak.Internal.Prelude
import Riak.Internal.Utils (int2bs)

---- | An exact query on a secondary index.
data ExactQuery
  = Binary !ByteString !ByteString
  | Integer !ByteString !Int64
  deriving stock (Show)

name :: ExactQuery -> ByteString
name = \case
  Binary n _ -> n <> "_bin"
  Integer n _ -> n <> "_int"

value :: ExactQuery -> ByteString
value = \case
  Binary _ v -> v
  Integer _ v -> int2bs v
