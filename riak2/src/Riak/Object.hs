module Riak.Object
  ( Object(..)
  ) where

import Riak.Charset          (Charset)
import Riak.ContentEncoding  (ContentEncoding)
import Riak.ContentType      (ContentType)
import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Key              (Key)
import Riak.Metadata         (Metadata)
import Riak.TTL              (TTL)
import Riak.Vtag             (Vtag)

data Object
  = Object
  { key :: Key
  , value :: ByteString
  , contentType :: Maybe ContentType
  , charset :: Maybe Charset
  , contentEncoding :: Maybe ContentEncoding
  , vtag :: Maybe Vtag
  , lastModified :: Maybe UTCTime
  , metadata :: Metadata
  , indexes :: [Index]
  , deleted :: Bool
  , ttl :: TTL
  } deriving stock (Eq, Generic, Show)
