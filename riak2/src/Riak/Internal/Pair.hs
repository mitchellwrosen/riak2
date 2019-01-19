module Riak.Internal.Pair where

import Riak.Internal.Prelude
import Riak.Proto

import qualified Riak.Proto.Lens as L


fromTuple :: (ByteString, Maybe ByteString) -> RpbPair
fromTuple (key, value) =
  defMessage
    & L.key .~ key
    & L.maybe'value .~ value

toTuple :: RpbPair -> (ByteString, Maybe ByteString)
toTuple pair =
  (pair ^. L.key, pair ^. L.maybe'value)
