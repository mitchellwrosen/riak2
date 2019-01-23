module Riak.Client
  ( -- * Client
    Client
  , new
    -- * Object operations
    -- ** Get object
  , get
  , getHead
  , getIfModified
  , getHeadIfModified
  , IfModified(..)
    -- ** Put object
  , put
  , putGet
  , putGetHead
    -- ** Delete object
  , delete
  ) where

import Riak.Content          (Content(..))
import Riak.Interface        (Interface, Result(..))
import Riak.Internal.Prelude
import Riak.Internal.Vclock  (Vclock(..))
import Riak.Key              (Key(..))
import Riak.Object           (Object(..))
import Riak.Opts             (GetOpts(..), PutOpts(..))
import Riak.Proto

import qualified Riak.Interface       as Interface
import qualified Riak.Internal.Index  as Index
import qualified Riak.Internal.Object as Object
import qualified Riak.Internal.Pair   as Pair
import qualified Riak.Internal.Quorum as Quorum
import qualified Riak.Proto.Lens      as L

import qualified Data.ByteString as ByteString


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
-- 'put'.
get
  :: MonadIO m
  => Client -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Result [Object ByteString])
get client key opts = liftIO $
  (fmap.fmap)
    (Object.fromGetResp key)
    (Interface.get (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts

-- | Get an object's metadata.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'putRiakObject'.
getHead
  :: MonadIO m
  => Client -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Result [Object ()])
getHead client key opts = liftIO $
  (fmap.fmap)
    (map (() <$) . Object.fromGetResp key)
    (Interface.get (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.head .~ True

-- | Get an object if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
getIfModified
  :: MonadIO m
  => Client -- ^
  -> Content a -- ^
  -> GetOpts -- ^
  -> m (Result (IfModified [Object ByteString]))
getIfModified client (Content { key, vclock }) opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Unmodified
        else Modified (Object.fromGetResp key response))
    (Interface.get (iface client) request)

  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.ifModified .~ unVclock vclock

-- | Get an object's metadata if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
getHeadIfModified
  :: MonadIO m
  => Client -- ^
  -> Content a -- ^
  -> GetOpts -- ^
  -> m (Result (IfModified [Object ()]))
getHeadIfModified client (Content { key, vclock }) opts = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.get (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.head .~ True
        & L.ifModified .~ unVclock vclock

    fromResponse :: RpbGetResp -> IfModified [Object ()]
    fromResponse response =
      if response ^. L.unchanged
        then Unmodified
        else Modified ((() <$) <$> Object.fromGetResp key response)

makeRpbGetReq :: Key -> GetOpts -> RpbGetReq
makeRpbGetReq key opts =
  defMessage
    & L.bucket .~ (key ^. field @"bucket")
    & L.deletedvclock .~ True
    & L.head .~ True
    & L.key .~ (key ^. field @"key")
    & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
    & L.maybe'nVal .~ Quorum.toWord32 (opts ^. field @"n")
    & L.maybe'notfoundOk .~ defTrue (notfoundOk opts)
    & L.maybe'pr .~ Quorum.toWord32 (pr opts)
    & L.maybe'r .~ Quorum.toWord32 (r opts)
    & L.maybe'sloppyQuorum .~ defFalse (opts ^. field @"sloppyQuorum")
    & L.maybe'timeout .~ (opts ^. field @"timeout")
    & L.type' .~ key ^. field @"type'"


-- | Put an object and return its key.
--
-- If you set its key component to @""@, Riak will randomly generate one.
put ::
     MonadIO m
  => Client -- ^
  -> Content ByteString -- ^
  -> PutOpts -- ^
  -> m (Result Key)
put client content opts = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.put (iface client) request)
  where
    request :: RpbPutReq
    request =
      makeRpbPutReq key content opts

    key@(Key type' bucket k) =
      content ^. field @"key"

    fromResponse :: RpbPutResp -> Key
    fromResponse response =
      if ByteString.null k
        then Key type' bucket (response ^. L.key)
        else key

-- | Put an object and return it.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
putGet ::
     MonadIO m
  => Client -- ^
  -> Content ByteString -- ^
  -> PutOpts -- ^
  -> m (Result (NonEmpty (Object ByteString)))
putGet client content opts = liftIO $
  (fmap.fmap)
    (Object.fromPutResp key)
    (Interface.put (iface client) request)

  where
    request :: RpbPutReq
    request =
      makeRpbPutReq key content opts
        & L.returnBody .~ True

    key :: Key
    key =
      content ^. field @"key"

-- | Put an object and return its metadata.
--
-- If multiple siblings are returned, you should perform a 'get', resolve them,
-- then perform a 'put'.
putGetHead ::
     MonadIO m
  => Client -- ^
  -> Content ByteString -- ^
  -> PutOpts -- ^
  -> m (Result (NonEmpty (Object ())))
putGetHead client content opts = liftIO $
  (fmap.fmap)
    (fmap (() <$) . Object.fromPutResp key)
    (Interface.put (iface client) request)

  where
    request :: RpbPutReq
    request =
      makeRpbPutReq key content opts
        & L.returnHead .~ True

    key :: Key
    key =
      content ^. field @"key"

makeRpbPutReq ::
     Key
  -> Content ByteString
  -> PutOpts
  -> RpbPutReq
makeRpbPutReq (Key type' bucket key) content opts =
  defMessage
    & L.bucket .~ bucket
    & L.content .~
        (defMessage
          & L.indexes .~ map Index.toPair (content ^. field @"indexes")
          & L.maybe'charset .~ (content ^. field @"charset")
          & L.maybe'contentEncoding .~ (content ^. field @"encoding")
          & L.maybe'contentType .~ (content ^. field @"type'")
          & L.usermeta .~ map Pair.fromTuple (content ^. field @"metadata")
          & L.value .~ (content ^. field @"value")
        )
    & L.maybe'dw .~ Quorum.toWord32 (dw opts)
    & L.maybe'key .~
        (if ByteString.null key
          then Nothing
          else Just key)
    & L.maybe'nVal .~ Quorum.toWord32 (opts ^. field @"n")
    & L.maybe'pw .~ Quorum.toWord32 (pw opts)
    & L.maybe'vclock .~
        (let
          vclock :: ByteString
          vclock =
            unVclock (content ^. field @"vclock")
        in
          if ByteString.null vclock
            then Nothing
            else Just vclock)
    & L.maybe'w .~ Quorum.toWord32 (w opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")
    & L.maybe'sloppyQuorum .~ defFalse (opts ^. field @"sloppyQuorum")
    & L.type' .~ type'

delete ::
     MonadIO m
  => Client
  -> Content a
  -> m (Result ())
delete client content = liftIO $
  (fmap.fmap)
    (\RpbDelResp -> ())
    (Interface.delete (iface client) request)

  where
    request :: RpbDelReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.key .~ key
        -- TODO delete opts
        -- & L.maybe'dw .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'pw .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'rw .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined
        -- & L.maybe'w .~ undefined
        & L.type' .~ type'
        & L.vclock .~ unVclock (content ^. field @"vclock")

    Key type' bucket key =
      content ^. field @"key"

defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True

defTrue :: Bool -> Maybe Bool
defTrue = \case
  False -> Just False
  True -> Nothing
