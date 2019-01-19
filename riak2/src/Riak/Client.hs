module Riak.Client
  ( Client
  , new
  ) where

import Riak.Interface        (Interface, Result(..))
import Riak.Internal.Prelude
import Riak.Key              (Key, pattern Location)
import Riak.Object           (ObjectR(..))
import Riak.Opts             (GetObjectOpts(..))
import Riak.Proto
import Riak.Vtag             (Vtag(..))

import qualified Riak.Interface        as Interface
import qualified Riak.Internal.Content as Content
import qualified Riak.Internal.Quorum  as Quorum
import qualified Riak.Proto.Lens       as L

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


newtype Client
  = Client
  { iface :: Interface
  }

new :: Interface -> Client
new =
  Client

getObject
  :: MonadIO m
  => Client
  -> Key
  -> GetObjectOpts
  -> m (Result [ObjectR])
getObject client (Location typ bucket key) opts = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getObject (iface client) request)

  where
    request :: RpbGetReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.deletedvclock .~ True
        & L.key .~ key
        & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
        & L.maybe'nVal .~ Quorum.toWord32 (n opts)
        & L.maybe'notfoundOk .~ defTrue (notfoundOk opts)
        & L.maybe'pr .~ Quorum.toWord32 (pr opts)
        & L.maybe'r .~ Quorum.toWord32 (r opts)
        & L.maybe'sloppyQuorum .~ defFalse (sloppyQuorum opts)
        & L.maybe'timeout .~ timeout opts
        & L.type' .~ typ

    fromResponse :: RpbGetResp -> [ObjectR]
    fromResponse response =
      mapMaybe (fromContent (response ^. L.vclock)) (response ^. L.content)

    fromContent :: ByteString -> RpbContent -> Maybe ObjectR
    fromContent vclock content = do
      guard (not (content ^. L.deleted))
      pure ObjectR
        { value = content ^. L.value
        , contentType = content ^. L.maybe'contentType
        , charset = content ^. L.maybe'charset
        , contentEncoding = content ^. L.maybe'contentEncoding
        , vtag = coerce (content ^. L.maybe'vtag)
        , lastModified = Content.lastModified content
        , metadata = Content.metadata content
        , indexes = Content.indexes content
        , deleted = content ^. L.deleted
        , ttl = content ^. L.maybe'ttl
        }

defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True

defTrue :: Bool -> Maybe Bool
defTrue = \case
  False -> Just False
  True -> Nothing
