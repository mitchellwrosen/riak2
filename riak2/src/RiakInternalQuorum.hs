module RiakInternalQuorum where

import RiakInternalPrelude

import Data.Default.Class (Default(..))


-- | How many vnodes must respond before an operation is considered successful.
data Quorum
  = QuorumDefault
  | QuorumQuorum
  | QuorumAll
  | QuorumOf Natural -- ^ Must be @<= N@
  deriving stock (Eq)

instance Default Quorum where
  def :: Quorum
  def =
    QuorumDefault

instance Show Quorum where
  show :: Quorum -> [Char]
  show = \case
    QuorumDefault -> "default"
    QuorumQuorum -> "quorum"
    QuorumAll -> "all"
    QuorumOf n -> show n

fromWord32 :: Word32 -> Quorum
fromWord32 = \case
  4294967291 -> QuorumDefault
  4294967292 -> QuorumAll
  4294967293 -> QuorumQuorum
  4294967294 -> QuorumOf 1
  n          -> QuorumOf (fromIntegral n)

toWord32 :: Quorum -> Word32
toWord32 = \case
  QuorumDefault -> 4294967291
  QuorumQuorum  -> 4294967293
  QuorumAll     -> 4294967292
  QuorumOf n    -> fromIntegral n
