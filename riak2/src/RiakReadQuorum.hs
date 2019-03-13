module RiakReadQuorum where

import RiakQuorum (Quorum)

import qualified RiakQuorum as Quorum

import Control.Lens                       ((.~), (^.))
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')

import qualified Data.Riak.Proto as Proto


data ReadQuorum
  = ReadQuorum
  { nodes :: Quorum
  , notfoundOk :: NotfoundOk
  , primary :: Quorum
  } deriving stock (Eq, Generic, Show)

-- | How the coordinating node of a read request reacts to a "notfound" response
-- from another node.
--
-- * 'NotfoundOk'. The missing replica is treated as a positive assertion that
--   the data does not exist.
--
-- * 'NotfoundNotOk'. The coordinating node will keep waiting for other nodes to
--   reply so long as it is still able to satisfy the request's @nodes@ value.
--
-- * 'NotfoundNotOkBasic'. The coordinating node will keep waiting for other
--   nodes to reply so long as it is both still able to satisfy the request's
--   @nodes@ value /and/ it has not already heard from over half of the @nodes@.
data NotfoundOk
  = NotfoundOk
  | NotfoundNotOk
  | NotfoundNotOkBasic
  deriving stock (Eq, Show)

fromProto ::
     ( HasLens' a "basicQuorum" Bool
     , HasLens' a "maybe'notfoundOk" (Maybe Bool)
     , HasLens' a "pr" Word32
     , HasLens' a "r" Word32
     )
  => a
  -> ReadQuorum
fromProto proto =
  ReadQuorum
    { nodes = Quorum.fromWord32 (proto ^. Proto.r)
    , notfoundOk = notfoundOk
    , primary = Quorum.fromWord32 (proto ^. Proto.pr)
    }
  where
    notfoundOk :: NotfoundOk
    notfoundOk =
      if fromMaybe True (proto ^. Proto.maybe'notfoundOk) then
        NotfoundOk
      else if proto ^. Proto.basicQuorum then
        NotfoundNotOkBasic
      else
        NotfoundNotOk

setProto ::
     forall a.
     ( HasLens' a "basicQuorum" Bool
     , HasLens' a "notfoundOk" Bool
     , HasLens' a "pr" Word32
     , HasLens' a "r" Word32
     )
  => Maybe ReadQuorum
  -> a
  -> a
setProto = \case
  Nothing ->
    id

  Just (ReadQuorum nodes notfoundOk primary) ->
    setNotfoundOk notfoundOk .
    (Proto.pr .~ (Quorum.toWord32 primary)) .
    (Proto.r .~ (Quorum.toWord32 nodes))

  where
    setNotfoundOk :: NotfoundOk -> a -> a
    setNotfoundOk = \case
      NotfoundOk ->
        Proto.notfoundOk .~ True
      NotfoundNotOk ->
        (Proto.notfoundOk .~ False) .
        (Proto.basicQuorum .~ False)
      NotfoundNotOkBasic ->
        (Proto.notfoundOk .~ False) .
        (Proto.basicQuorum .~ True)
