module RiakReadQuorum where

import RiakQuorum (Quorum)

import qualified Libriak.Proto as Proto
import qualified RiakQuorum    as Quorum

import Control.Lens ((.~), (^.))


data ReadQuorum
  = ReadQuorum
  { nodes :: !Quorum
  , primary :: !Quorum
  } deriving stock (Generic, Show)

fromProto ::
     ( Proto.HasLens' a "pr" Word32
     , Proto.HasLens' a "r" Word32
     )
  => a
  -> ReadQuorum
fromProto proto =
  ReadQuorum
    { nodes = Quorum.fromWord32 (proto ^. Proto.r)
    , primary = Quorum.fromWord32 (proto ^. Proto.pr)
    }

setProto ::
     ( Proto.HasLens' a "pr" Word32
     , Proto.HasLens' a "r" Word32
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
