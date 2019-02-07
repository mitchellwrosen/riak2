module Riak.Content
  ( Content(..)
  , newContent
  ) where

import Riak.Internal.Context        (Context, newContext)
import Riak.Internal.Key            (Key)
import Riak.Internal.Prelude
import Riak.Internal.SecondaryIndex (SecondaryIndex)


-- | Object content. This is data that is provided when reading and writing an
-- object.
data Content a
  = Content
  { charset :: !(Maybe ByteString) -- ^ Charset
  , context :: !Context -- ^ Causal context
  , encoding :: !(Maybe ByteString) -- ^ Content encoding
  , indexes :: ![SecondaryIndex] -- ^ Secondary indexes
  , key :: !Key -- ^ Key
  , metadata :: ![(ByteString, Maybe ByteString)] -- ^ User metadata
  , type' :: !(Maybe ByteString) -- ^ Content type
  , ttl :: !(Maybe Word32) -- ^ Time to live. Unused on write. TODO NominalDiffTime
  , value :: !a -- ^ Value
  } deriving stock (Eq, Functor, Generic, Show)

newContent :: Key -> a -> Content a
newContent key value =
  Content
    { charset = Nothing
    , context = newContext
    , encoding = Nothing
    , indexes = []
    , key = key
    , metadata = []
    , ttl = Nothing
    , type' = Nothing
    , value = value
    }
