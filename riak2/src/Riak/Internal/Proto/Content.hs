module Riak.Internal.Proto.Content where

import Riak.Internal.Prelude
import Riak.Proto

import qualified Riak.Internal.Pair as Pair
import qualified Riak.Proto.Lens    as L

import Data.Time             (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


lastModified :: RpbContent -> UTCTime
lastModified content =
  posixSecondsToUTCTime (seconds + microseconds)

  where
    seconds :: NominalDiffTime
    seconds =
      fromIntegral (content ^. L.lastMod)

    microseconds :: NominalDiffTime
    microseconds =
      realToFrac (content ^. L.lastModUsecs) / 1000000

metadata :: RpbContent -> [(ByteString, Maybe ByteString)]
metadata content =
  map Pair.toTuple (content ^. L.usermeta)
