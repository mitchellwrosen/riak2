module RiakProtoPair where

import Control.Lens ((.~), (^.))

import qualified Data.Riak.Proto as Proto


fromTuple :: (ByteString, ByteString) -> Proto.RpbPair
fromTuple (key, value) =
  Proto.defMessage
    & Proto.key .~ key
    & Proto.value .~ value

toTuple :: Proto.RpbPair -> (ByteString, ByteString)
toTuple pair =
  (pair ^. Proto.key, pair ^. Proto.value)
