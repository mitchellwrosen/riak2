module RiakWriteQuorum where

import RiakQuorum (Quorum)

import qualified Libriak.Proto as Proto
import qualified RiakQuorum    as Quorum

import Control.Lens ((.~), (^.))


data WriteQuorum
  = WriteQuorum
  { durable :: !Quorum
  , nodes :: !Quorum
  , primary :: !Quorum
  } deriving stock (Generic, Show)

fromProto ::
     ( Proto.HasLens' a "dw" Word32
     , Proto.HasLens' a "pw" Word32
     , Proto.HasLens' a "w" Word32
     )
  => a
  -> WriteQuorum
fromProto proto =
  WriteQuorum
    { durable = Quorum.fromWord32 (proto ^. Proto.dw)
    , nodes = Quorum.fromWord32 (proto ^. Proto.w)
    , primary = Quorum.fromWord32 (proto ^. Proto.pw)
    }

setProto ::
     ( Proto.HasLens' a "dw" Word32
     , Proto.HasLens' a "pw" Word32
     , Proto.HasLens' a "w" Word32
     )
  => Maybe WriteQuorum
  -> a
  -> a
setProto = \case
  Nothing ->
    id

  Just (WriteQuorum durable nodes primary) ->
    (Proto.dw .~ Quorum.toWord32 durable) .
    (Proto.pw .~ Quorum.toWord32 primary) .
    (Proto.w .~ Quorum.toWord32 nodes)
