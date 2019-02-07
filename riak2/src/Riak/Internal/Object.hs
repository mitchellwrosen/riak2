module Riak.Internal.Object
  ( Object(..)
  , fromGetResponse
  , fromPutResponse
  ) where

import Riak.Content          (Content(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Internal.Proto.Content  as Proto.Content
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex
import qualified Riak.Proto                   as Proto
import qualified Riak.Proto.Lens              as L

import Control.Lens ((^.))
import Data.Time    (UTCTime)

import qualified ByteString
import qualified Data.List.NonEmpty as List1


data Object a
  = Object
  { content :: Content a
  , deleted :: Bool
  , lastModified :: UTCTime
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  } deriving stock (Eq, Functor, Generic, Show)

fromProtoContent ::
     Key
  -> ByteString
  -> Proto.Content
  -> Object ByteString
fromProtoContent key context proto =
  Object
    { content =
        Content
          { charset = proto ^. L.maybe'charset
          , context = Context context
          , encoding = proto ^. L.maybe'contentEncoding
          , indexes = map SecondaryIndex.fromPair (proto ^. L.indexes)
          , key = key
          , metadata = Proto.Content.metadata proto
          , type' = proto ^. L.maybe'contentType
          , value = proto ^. L.value
          }
    , deleted = proto ^. L.deleted
    , lastModified = Proto.Content.lastModified proto
    , ttl = proto ^. L.maybe'ttl
    }

-- | Parse a list of objects from a get response.
fromGetResponse :: Key -> Proto.GetResponse -> [Object ByteString]
fromGetResponse key response =
  fromProtoContent key (response ^. L.context) <$>
    response ^. L.content

-- | Parse an object from a put response.
--
-- Assumes that either @return_body@ or @return_head@ was set on the request.
fromPutResponse :: Key -> Proto.PutResponse -> NonEmpty (Object ByteString)
fromPutResponse k@(Key type' bucket key) response =
  List1.fromList
    (fromProtoContent key' (response ^. L.context) <$>
      response ^. L.content)
  where
    key' :: Key
    key' =
      if ByteString.null key
        then Key type' bucket (response ^. L.key)
        else k
