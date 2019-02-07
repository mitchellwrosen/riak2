module Riak.Internal.Quorum where

import Riak.Internal.Prelude

import Data.Default.Class (Default(..))


-- | How many vnodes must respond before an operation is considered successful.
-- TODO prefix constructors with Quorum
data Quorum
  = Default
  | Quorum
  | All
  | Of Natural -- ^ Must be @<= N@
  deriving stock (Eq, Show)

instance Default Quorum where
  def = Default

toWord32 :: Quorum -> Word32
toWord32 = \case
  Default -> 4294967291
  Quorum  -> 4294967293
  All     -> 4294967292
  Of n    -> fromIntegral n
