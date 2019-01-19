module Riak.Object
  ( ObjectR(..)
  , ObjectW(..)
  ) where

import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Key              (Key)
import Riak.Vclock           (Vclock)
import Riak.Vtag             (Vtag)

data ObjectR a
  = ObjectR
  { charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , contentType :: Maybe ByteString
  , deleted :: Bool
  , indexes :: [Index]
  , lastModified :: Maybe UTCTime
  , metadata :: [(ByteString, Maybe ByteString)]
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  , value :: a
  , vclock :: Vclock
  , vtag :: Maybe Vtag
  }

data ObjectW
  = ObjectW
  { charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , contentType :: Maybe ByteString
  , indexes :: [Index]
  , metadata :: [(ByteString, Maybe ByteString)]
  , value :: ByteString
  , vclock :: Vclock
  }
