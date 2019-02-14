module Riak.Internal.Proto.Content where

import Riak.Internal.Prelude

import qualified Libriak.Proto            as Proto
import qualified Riak.Internal.Proto.Pair as Pair

import Control.Lens          ((^.))
import Data.Time             (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


lastModified :: Proto.RpbContent -> UTCTime
lastModified content =
  posixSecondsToUTCTime (seconds + microseconds)

  where
    seconds :: NominalDiffTime
    seconds =
      fromIntegral (content ^. Proto.lastMod)

    microseconds :: NominalDiffTime
    microseconds =
      realToFrac (content ^. Proto.lastModUsecs) / 1000000

metadata :: Proto.RpbContent -> [(ByteString, ByteString)]
metadata content =
  map Pair.toTuple (content ^. Proto.usermeta)
