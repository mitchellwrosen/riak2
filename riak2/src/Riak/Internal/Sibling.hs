module Riak.Internal.Sibling where

import Riak.Content          (Content(..))
import Riak.Internal.Prelude

import Control.Lens ((^.))
import Data.Time    (UTCTime)

import qualified Libriak.Proto                as Proto
import qualified Libriak.Proto.Lens           as L
import qualified Riak.Internal.Proto.Content  as Proto.Content
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex

data Sibling a
  = Sibling !(Content a)
  | Tombstone !UTCTime
  deriving stock (Eq, Functor, Show)

fromProtoContent :: Proto.Content -> Sibling ByteString
fromProtoContent content =
  if content ^. L.deleted
    then
      Tombstone (Proto.Content.lastModified content)

    else
      Sibling Content
        { charset = content ^. L.maybe'charset
        , encoding = content ^. L.maybe'contentEncoding
        , indexes = map SecondaryIndex.fromPair (content ^. L.indexes)
        , lastModified = Proto.Content.lastModified content
        , metadata = Proto.Content.metadata content
        , ttl = content ^. L.maybe'ttl
        , type' = content ^. L.maybe'contentType
        , value = content ^. L.value
        }
