module Riak.Content
  ( Content(..)
  , newContent
  ) where

import Riak.Internal.Prelude
import Riak.Internal.SecondaryIndex (SecondaryIndex)


-- | Object content. This is data that is provided when reading and writing an
-- object.
data Content a
  = Content
  { charset :: !(Maybe ByteString) -- ^ Charset
  , encoding :: !(Maybe ByteString) -- ^ Content encoding
  , indexes :: ![SecondaryIndex] -- ^ Secondary indexes
  , metadata :: ![(ByteString, Maybe ByteString)] -- ^ User metadata
  , type' :: !(Maybe ByteString) -- ^ Content type
  , ttl :: !(Maybe Word32) -- ^ Time to live. Unused on write. TODO NominalDiffTime
  , value :: !a -- ^ Value
  } deriving stock (Eq, Functor, Generic, Show)

newContent :: a -> Content a
newContent value =
  Content
    { charset = Nothing
    , encoding = Nothing
    , indexes = []
    , metadata = []
    , ttl = Nothing
    , type' = Nothing
    , value = value
    }
