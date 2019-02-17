module RiakSecondaryIndexValue where

import RiakUtils (int2bs)


-- | A secondary index value.
-- TODO change constructor names?
data SecondaryIndexValue :: Type -> Type where
  Binary :: !ByteString -> SecondaryIndexValue ByteString
  Integer :: !Int64 -> SecondaryIndexValue Int64

deriving stock instance Show (SecondaryIndexValue a)

encode :: SecondaryIndexValue a -> ByteString
encode = \case
  Binary value -> value
  Integer value -> int2bs value
