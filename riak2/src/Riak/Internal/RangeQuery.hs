module Riak.Internal.RangeQuery where

import Riak.Internal.Prelude
import Riak.Internal.Utils   (int2bs)

-- | A range query on a secondary index.
data RangeQuery :: Type -> Type where
  Binary :: !ByteString -> !ByteString -> !ByteString -> RangeQuery ByteString
  Integer :: !ByteString -> !Int64 -> !Int64 -> RangeQuery Int64

deriving stock instance Show (RangeQuery a)

name :: RangeQuery a -> ByteString
name = \case
  Binary n _ _ -> n <> "_bin"
  Integer n _ _ -> n <> "_int"

minValue :: RangeQuery a -> ByteString
minValue = \case
  Binary _ v _ -> v
  Integer _ v _ -> int2bs v

maxValue :: RangeQuery a -> ByteString
maxValue = \case
  Binary _ _ v -> v
  Integer _ _ v -> int2bs v
