module Riak.Internal.ObjectW where

import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Vclock           (Vclock)


data ObjectW
  = ObjectW
  { charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , contentType :: Maybe ByteString
  , indexes :: [Index]
  , metadata :: [(ByteString, Maybe ByteString)]
  , value :: ByteString
  , vclock :: Maybe Vclock
  } deriving stock (Generic, Show)

