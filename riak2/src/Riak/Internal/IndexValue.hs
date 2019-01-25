module Riak.Internal.IndexValue where

import Riak.Internal.Prelude
import Riak.Internal.Utils (int2bs)


data IndexValue :: Type -> Type where
  Binary :: !ByteString -> IndexValue ByteString
  Integer :: !Int64 -> IndexValue Int64

deriving stock instance Show (IndexValue a)

encode :: IndexValue a -> ByteString
encode = \case
  Binary value -> value
  Integer value -> int2bs value
