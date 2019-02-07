module Riak.Internal.Proto.Pair where

import Riak.Internal.Prelude
import Riak.Proto

import qualified Riak.Proto.Lens as L

import Control.Lens ((.~), (^.))


fromTuple :: (ByteString, Maybe ByteString) -> Pair
fromTuple (key, value) =
  defMessage
    & L.key .~ key
    & L.maybe'value .~ value

toTuple :: Pair -> (ByteString, Maybe ByteString)
toTuple pair =
  (pair ^. L.key, pair ^. L.maybe'value)

