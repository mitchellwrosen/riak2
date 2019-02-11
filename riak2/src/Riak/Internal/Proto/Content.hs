module Riak.Internal.Proto.Content where

import Riak.Internal.Prelude

import qualified Libriak.Proto            as Proto
import qualified Libriak.Proto.Lens       as L
import qualified Riak.Internal.Proto.Pair as Pair

import Control.Lens          ((^.))
import Data.Time             (NominalDiffTime, UTCTime)
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
