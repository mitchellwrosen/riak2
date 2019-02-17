module RiakSibling where

import RiakContent (Content(..))

import Control.Lens ((^.))
import Data.Time    (UTCTime)

import qualified Libriak.Proto      as Proto
import qualified RiakProtoContent   as Proto.Content
import qualified RiakSecondaryIndex as SecondaryIndex

data Sibling a
  = Sibling !(Content a)
  | Tombstone !UTCTime
  deriving stock (Eq, Functor, Show)

fromProtoContent :: Proto.RpbContent -> Sibling ByteString
fromProtoContent content =
  if content ^. Proto.deleted
    then
      Tombstone (Proto.Content.lastModified content)

    else
      Sibling Content
        { charset = content ^. Proto.maybe'charset
        , encoding = content ^. Proto.maybe'contentEncoding
        , indexes = map SecondaryIndex.fromPair (content ^. Proto.indexes)
        , lastModified = Proto.Content.lastModified content
        , metadata = Proto.Content.metadata content
        , ttl = content ^. Proto.maybe'ttl
        , type' = content ^. Proto.maybe'contentType
        , value = content ^. Proto.value
        }
