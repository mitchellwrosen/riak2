module Riak.Internal.Object
  ( Object(..)
  , fromGetResp
  , fromPutResp
  ) where

import Riak.Content          (Content(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Metadata         (Metadata(..))
import Riak.Proto
import Riak.Vclock           (Vclock(..))

import qualified Riak.Internal.Index         as Index
import qualified Riak.Internal.Proto.Content as Proto.Content
import qualified Riak.Proto.Lens             as L

import qualified Data.ByteString    as ByteString
import qualified Data.List.NonEmpty as List1


data Object a
  = Object
  { content :: Content a
  , metadata :: Metadata
  } deriving stock (Functor, Generic, Show)

fromProtoContent ::
     Key
  -> ByteString
  -> RpbContent
  -> Object ByteString
fromProtoContent key vclock proto =
  Object
    { content =
        Content
          { charset = proto ^. L.maybe'charset
          , encoding = proto ^. L.maybe'contentEncoding
          , indexes = map Index.fromPair (proto ^. L.indexes)
          , key = key
          , metadata = Proto.Content.metadata proto
          , type' = proto ^. L.maybe'contentType
          , value = proto ^. L.value
          , vclock = Vclock vclock
          }
    , metadata =
        Metadata
          { deleted = proto ^. L.deleted
          , lastModified = Proto.Content.lastModified proto
          , ttl = proto ^. L.maybe'ttl
          }
    }

fromGetResp :: Key -> RpbGetResp -> [Object ByteString]
fromGetResp key response =
  fromProtoContent key (response ^. L.vclock) <$>
    response ^. L.content

-- | Parse an object from a put response.
--
-- Assumes that either @return_body@ or @return_head@ was set on the request.
fromPutResp :: Key -> RpbPutResp -> NonEmpty (Object ByteString)
fromPutResp k@(Key type' bucket key) response =
  List1.fromList
    (fromProtoContent key' (response ^. L.vclock) <$>
      response ^. L.content)
  where
    key' :: Key
    key' =
      if ByteString.null key
        then Key type' bucket (response ^. L.key)
        else k
