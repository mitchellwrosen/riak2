module Riak.Internal.Content where

import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Key              (Key)
import Riak.Vclock           (Vclock)


data Content a
  = Content
  { charset :: Maybe ByteString
  , encoding :: Maybe ByteString
  , indexes :: [Index]
  , key :: Key
  , metadata :: [(ByteString, Maybe ByteString)]
  , type' :: Maybe ByteString
  , value :: a
  , vclock :: Vclock
  } deriving stock (Functor, Generic, Show)
