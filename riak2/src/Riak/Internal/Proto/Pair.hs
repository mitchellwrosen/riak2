module Riak.Internal.Proto.Pair where

import Libriak.Proto
import Riak.Internal.Prelude

import qualified Libriak.Proto.Lens as L

import Control.Lens ((.~), (^.))


fromTuple :: (ByteString, ByteString) -> Pair
fromTuple (key, value) =
  defMessage
    & L.key .~ key
    & L.value .~ value

toTuple :: Pair -> (ByteString, ByteString)
toTuple pair =
  (pair ^. L.key, pair ^. L.value)
