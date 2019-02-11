module Riak.Internal.Proto.Pair where

import Libriak.Proto
import Riak.Internal.Prelude

import qualified Libriak.Proto.Lens as L

import Control.Lens ((.~), (^.))


fromTuple :: (ByteString, Maybe ByteString) -> Pair
fromTuple (key, value) =
  defMessage
    & L.key .~ key
    & L.maybe'value .~ value

toTuple :: Pair -> (ByteString, Maybe ByteString)
toTuple pair =
  (pair ^. L.key, pair ^. L.maybe'value)

