module RiakProtoContent where

import qualified Libriak.Proto as Proto
import qualified RiakProtoPair as Pair

import Control.Lens          ((.~), (^.))
import Data.List             (foldl')
import Data.Time             (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.HashMap.Strict as HashMap


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

metadata :: Proto.RpbContent -> HashMap ByteString ByteString
metadata content =
  foldl'
    (\acc pair -> HashMap.insert (pair ^. Proto.key) (pair ^. Proto.value) acc)
    HashMap.empty
    (content ^. Proto.usermeta)

setMetadata ::
     HashMap ByteString ByteString
  -> Proto.RpbContent
  -> Proto.RpbContent
setMetadata metadata =
  Proto.usermeta .~
    HashMap.foldlWithKey'
      (\acc key value ->
        (Proto.defMessage
          & Proto.key .~ key
          & Proto.value .~ value)
        : acc)
      []
      metadata