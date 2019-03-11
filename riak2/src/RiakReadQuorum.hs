module RiakReadQuorum where

import RiakQuorum (Quorum)

import qualified RiakQuorum as Quorum

import Control.Lens                       ((.~), (^.))
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')

import qualified Data.Riak.Proto as Proto


data ReadQuorum
  = ReadQuorum
  { nodes :: !Quorum
  , primary :: !Quorum
  } deriving stock (Eq, Generic, Show)

fromProto ::
     ( HasLens' a "pr" Word32
     , HasLens' a "r" Word32
     )
  => a
  -> ReadQuorum
fromProto proto =
  ReadQuorum
    { nodes = Quorum.fromWord32 (proto ^. Proto.r)
    , primary = Quorum.fromWord32 (proto ^. Proto.pr)
    }

setProto ::
     ( HasLens' a "pr" Word32
     , HasLens' a "r" Word32
     )
  => Maybe ReadQuorum
  -> a
  -> a
setProto = \case
  Nothing ->
    id

  Just (ReadQuorum nodes primary) ->
    (Proto.pr .~ (Quorum.toWord32 primary)) .
    (Proto.r .~ (Quorum.toWord32 nodes))
