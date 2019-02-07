module Riak.Internal.Content where

import Riak.Internal.Context        (Context)
import Riak.Internal.Key            (Key)
import Riak.Internal.Prelude
import Riak.Internal.SecondaryIndex (SecondaryIndex)


-- | Object content. This is data that is provided when reading and writing an
-- object.
data Content a
  = Content
  { charset :: Maybe ByteString -- ^ Charset
  , context :: Context -- ^ Causal context
  , encoding :: Maybe ByteString -- ^ Content encoding
  , indexes :: [SecondaryIndex] -- ^ Secondary indexes
  , key :: Key -- ^ Key
  , metadata :: [(ByteString, Maybe ByteString)] -- ^ User metadata
  , type' :: Maybe ByteString -- ^ Content type
  , value :: a -- ^ Value
  } deriving stock (Eq, Functor, Generic, Show)
