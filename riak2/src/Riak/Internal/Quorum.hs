module Riak.Internal.Quorum where

import Riak.Internal.Prelude

import Data.Default.Class (Default(..))


-- | How many vnodes must respond before an operation is considered successful.
data Quorum
  = QuorumDefault
  | QuorumQuorum
  | QuorumAll
  | QuorumOf Natural -- ^ Must be @<= N@
  deriving stock (Eq, Show)

instance Default Quorum where
  def :: Quorum
  def =
    QuorumDefault

toWord32 :: Quorum -> Word32
toWord32 = \case
  QuorumDefault -> 4294967291
  QuorumQuorum  -> 4294967293
  QuorumAll     -> 4294967292
  QuorumOf n    -> fromIntegral n
