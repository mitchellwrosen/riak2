module Riak.Object
  ( ObjectR(..)
  , ObjectW(..)
  ) where

import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Key              (Key)
import Riak.Vtag             (Vtag)

data ObjectR
  = ObjectR
  { value :: ByteString
  , contentType :: Maybe ByteString
  , charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , vtag :: Maybe Vtag
  , lastModified :: Maybe UTCTime
  , metadata :: [(ByteString, Maybe ByteString)]
  , indexes :: [Index]
  , deleted :: Bool
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  }

data ObjectW
  = ObjectW
  { value :: ByteString
  , contentType :: Maybe ByteString
  , charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , metadata :: [(ByteString, Maybe ByteString)]
  , indexes :: [Index]
  }
