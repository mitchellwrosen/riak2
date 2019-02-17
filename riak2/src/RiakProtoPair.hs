module RiakProtoPair where

import qualified Libriak.Proto as Proto

import Control.Lens ((.~), (^.))


fromTuple :: (ByteString, ByteString) -> Proto.RpbPair
fromTuple (key, value) =
  Proto.defMessage
    & Proto.key .~ key
    & Proto.value .~ value

toTuple :: Proto.RpbPair -> (ByteString, ByteString)
toTuple pair =
  (pair ^. Proto.key, pair ^. Proto.value)
