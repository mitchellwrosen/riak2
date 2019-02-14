module Riak.Internal.Proto.Pair where

import Libriak.Proto
import Riak.Internal.Prelude

import qualified Libriak.Proto as Proto

import Control.Lens ((.~), (^.))


fromTuple :: (ByteString, ByteString) -> RpbPair
fromTuple (key, value) =
  defMessage
    & Proto.key .~ key
    & Proto.value .~ value

toTuple :: RpbPair -> (ByteString, ByteString)
toTuple pair =
  (pair ^. Proto.key, pair ^. Proto.value)
