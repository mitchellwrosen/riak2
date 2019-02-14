module Riak.Internal.Object where

import Riak.Internal.Context (Context(..), newContext)
import Riak.Internal.Prelude
import Riak.Internal.Sibling (Sibling(..))
import Riak.Key              (Key(..))

import qualified Libriak.Proto         as Proto
import qualified Riak.Internal.Sibling as Sibling

import Control.Lens ((^.))

import qualified ByteString
import qualified Data.List.NonEmpty as List1


data Object a
  = Object
  { content :: !a
  , context :: !Context -- ^ Causal context
  , key :: !Key -- ^ Key
  } deriving stock (Eq, Functor, Generic, Show)

newObject ::
     Key -- ^ Key
  -> a -- ^ Content
  -> Object a
newObject key content =
  Object
    { content = content
    , context = newContext
    , key = key
    }

-- | Parse an object from a get response.
fromGetResponse ::
     Key
  -> Proto.RpbGetResp
  -> Object [Sibling ByteString]
fromGetResponse key response =
  Object
    { content = map Sibling.fromProtoContent (response ^. Proto.content)
    , context = Context (response ^. Proto.vclock)
    , key = key
    }

-- | Parse an object from a put response.
--
-- Assumes that either @return_body@ or @return_head@ was set on the request.
fromPutResponse ::
     Key
  -> Proto.RpbPutResp
  -> Object (NonEmpty (Sibling ByteString))
fromPutResponse k@(Key bucketType bucket key) response =
  Object
    { content = List1.fromList (map Sibling.fromProtoContent (response ^. Proto.content))
    , context = Context (response ^. Proto.vclock)
    , key = key'
    }

  where
    key' :: Key
    key' =
      if ByteString.null key
        then Key bucketType bucket (response ^. Proto.key)
        else k
