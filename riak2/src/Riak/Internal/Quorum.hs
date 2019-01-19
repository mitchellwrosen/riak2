module Riak.Internal.Quorum where

import Riak.Internal.Prelude


-- | How many vnodes must respond before an operation is considered successful.
data Quorum
  = Default
  | Quorum
  | All
  | Of Natural -- ^ Must be @<= N@
  deriving stock (Eq, Show)

instance Default Quorum where
  def = Default

toWord32 :: Quorum -> Maybe Word32
toWord32 = \case
  Default -> Nothing -- 4294967291
  Quorum  -> Just 4294967293
  All     -> Just 4294967292
  Of n    -> Just (fromIntegral n)
