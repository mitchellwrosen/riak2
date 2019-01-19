module Riak.Internal.Content where

import Riak.Index            (Index(..))
import Riak.Internal.Panic   (impurePanic)
import Riak.Internal.Prelude
import Riak.Internal.Utils   (bs2int, int2bs)
import Riak.Proto

import qualified Riak.Internal.Pair as Pair
import qualified Riak.Proto.Lens    as L

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.ByteString as ByteString


indexes :: RpbContent -> [Index]
indexes content =
  map parseIndex (content ^. L.indexes)
  where
    parseIndex :: RpbPair -> Index
    parseIndex =
      Pair.toTuple >>> \case
        (ByteString.stripSuffix "_bin" -> Just k, Just v) ->
          IndexBin k v

        (ByteString.stripSuffix "_int" -> Just k, Just v) ->
          IndexInt k (bs2int v)

        (k, v) ->
          impurePanic "Riak.Internal.Content.parseIndex"
            ( ("key",   k)
            , ("value", v)
            )

lastModified :: RpbContent -> Maybe UTCTime
lastModified content = do
  secs <- content ^. L.maybe'lastMod
  usecs <- (content ^. L.maybe'lastModUsecs) <|> pure 0
  let usecs_d = realToFrac usecs / 1000000 :: Double
  pure (posixSecondsToUTCTime (fromIntegral secs + realToFrac usecs_d))

metadata :: RpbContent -> [(ByteString, Maybe ByteString)]
metadata content =
  map Pair.toTuple (content ^. L.usermeta)
