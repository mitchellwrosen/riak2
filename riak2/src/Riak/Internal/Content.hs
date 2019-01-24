module Riak.Internal.Content where

import Riak.Context          (Context)
import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Key              (Key)


-- | Object content. This is data that is provided when reading and writing an
-- object.
data Content a
  = Content
  { charset :: Maybe ByteString -- ^ Charset.
  , context :: Context -- ^ Causal context.
  , encoding :: Maybe ByteString -- ^ Content encoding.
  , indexes :: [Index] -- ^ Secondary indexes.
  , key :: Key -- ^ Key.
  , metadata :: [(ByteString, Maybe ByteString)] -- ^ User metadata.
  , type' :: Maybe ByteString -- ^ Content type.
  , value :: a -- ^ Value.
  } deriving stock (Functor, Generic, Show)
