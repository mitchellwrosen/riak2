module Riak.Client
  ( Client
  , new
  , getObject
  , getObjectHead
  , getObjectIfModified
  , getObjectHeadIfModified
  ) where

import Riak.Interface        (Interface, Result(..))
import Riak.Internal.Prelude
import Riak.Key              (Key, pattern Location)
import Riak.Object           (ObjectR(..))
import Riak.Opts             (GetObjectOpts(..))
import Riak.Proto
import Riak.Vclock           (Vclock(..))
import Riak.Vtag             (Vtag(..))

import qualified Riak.Interface        as Interface
import qualified Riak.Internal.Content as Content
import qualified Riak.Internal.Quorum  as Quorum
import qualified Riak.Proto.Lens       as L


newtype Client
  = Client
  { iface :: Interface
  }

new :: Interface -> Client
new =
  Client

data IfModified a
  = Modified a
  | Unmodified

-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putObject'.
getObject
  :: MonadIO m
  => Client
  -> Key
  -> GetObjectOpts
  -> m (Result [ObjectR ByteString])
getObject client key opts = liftIO $
  (fmap.fmap)
    (rpbGetRespToObjects (^. L.value))
    (Interface.getObject (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts

-- | Get an object's metadata.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
getObjectHead
  :: MonadIO m
  => Client
  -> Key
  -> GetObjectOpts
  -> m (Result [ObjectR ()])
getObjectHead client key opts = liftIO $
  (fmap.fmap)
    (rpbGetRespToObjects (const ()))
    (Interface.getObject (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.head .~ True

-- | Get an object if it has been modified since the given vclock.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putObject'.
getObjectIfModified
  :: MonadIO m
  => Client
  -> Key
  -> Vclock
  -> GetObjectOpts
  -> m (Result (IfModified [ObjectR ByteString]))
getObjectIfModified client key vclock opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Unmodified
        else Modified (rpbGetRespToObjects (^. L.value) response))
    (Interface.getObject (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.ifModified .~ unVclock vclock

-- | Get an object's metadata if it has been modified since the given vclock.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putObject'.
getObjectHeadIfModified
  :: MonadIO m
  => Client
  -> Key
  -> Vclock
  -> GetObjectOpts
  -> m (Result (IfModified [ObjectR ()]))
getObjectHeadIfModified client key vclock opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Unmodified
        else Modified (rpbGetRespToObjects (const ()) response))
    (Interface.getObject (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.head .~ True
        & L.ifModified .~ unVclock vclock

makeRpbGetReq :: Key -> GetObjectOpts -> RpbGetReq
makeRpbGetReq (Location typ bucket key) opts =
  defMessage
    & L.bucket .~ bucket
    & L.deletedvclock .~ True
    & L.head .~ True
    & L.key .~ key
    & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
    & L.maybe'nVal .~ Quorum.toWord32 (n opts)
    & L.maybe'notfoundOk .~ defTrue (notfoundOk opts)
    & L.maybe'pr .~ Quorum.toWord32 (pr opts)
    & L.maybe'r .~ Quorum.toWord32 (r opts)
    & L.maybe'sloppyQuorum .~ defFalse (sloppyQuorum opts)
    & L.maybe'timeout .~ timeout opts
    & L.type' .~ typ

rpbGetRespToObjects ::
     (RpbContent -> a)
  -> RpbGetResp
  -> [ObjectR a]
rpbGetRespToObjects value response =
  mapMaybe
    (rpbContentToObjectR (response ^. L.vclock) value)
    (response ^. L.content)

rpbContentToObjectR ::
     ByteString
  -> (RpbContent -> a)
  -> RpbContent
  -> Maybe (ObjectR a)
rpbContentToObjectR vclock value content = do
  guard (not (content ^. L.deleted))
  pure ObjectR
    { charset = content ^. L.maybe'charset
    , contentEncoding = content ^. L.maybe'contentEncoding
    , contentType = content ^. L.maybe'contentType
    , deleted = content ^. L.deleted
    , indexes = Content.indexes content
    , lastModified = Content.lastModified content
    , metadata = Content.metadata content
    , ttl = content ^. L.maybe'ttl
    , value = value content
    , vclock = Vclock vclock
    , vtag = coerce (content ^. L.maybe'vtag)
    }

defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True

defTrue :: Bool -> Maybe Bool
defTrue = \case
  False -> Just False
  True -> Nothing
