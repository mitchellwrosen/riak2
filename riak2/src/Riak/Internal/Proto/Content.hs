module Riak.Internal.Proto.Content where

import Riak.Internal.Prelude

import qualified Riak.Internal.Proto.Pair as Pair
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Data.Time             (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


lastModified :: Proto.Content -> UTCTime
lastModified content =
  posixSecondsToUTCTime (seconds + microseconds)

  where
    seconds :: NominalDiffTime
    seconds =
      fromIntegral (content ^. L.lastMod)

    microseconds :: NominalDiffTime
    microseconds =
      realToFrac (content ^. L.lastModUsecs) / 1000000

metadata :: Proto.Content -> [(ByteString, Maybe ByteString)]
metadata content =
  map Pair.toTuple (content ^. L.usermeta)
