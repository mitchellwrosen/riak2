module RiakWriteQuorum where

import RiakQuorum


data WriteQuorum
  = WriteQuorum
  { nodes :: !Quorum
  , primary :: !Quorum
  , durable :: !Quorum
  } deriving stock (Generic, Show)

