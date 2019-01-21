module Riak.Client
  ( -- * Client
    Client
  , new
    -- * Object operations
    -- ** Get object
  , getObject
  , getObjectHead
  , getObjectIfModified
  , getObjectHeadIfModified
  , IfModified(..)
    -- ** Put object
  , putObject
  , putObjectHead
  , putObjectBody
  , putNewObject
  , putNewObjectHead
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.Interface        (Interface, Result(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Object           (ObjectR(..), ObjectW(..))
import Riak.Opts             (GetObjectOpts(..), PutObjectOpts(..))
import Riak.Proto
import Riak.Vclock           (Vclock(..))

import qualified Riak.Interface        as Interface
import qualified Riak.Internal.Index   as Index
import qualified Riak.Internal.ObjectR as ObjectR
import qualified Riak.Internal.Pair    as Pair
import qualified Riak.Internal.Quorum  as Quorum
import qualified Riak.Proto.Lens       as L

import qualified Data.List.NonEmpty as List1


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
  => Client -- ^
  -> Key -- ^
  -> GetObjectOpts -- ^
  -> m (Result [ObjectR ByteString])
getObject client key opts = liftIO $
  (fmap.fmap)
    ObjectR.fromGetResp
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
  => Client -- ^
  -> Key -- ^
  -> GetObjectOpts -- ^
  -> m (Result [ObjectR ()])
getObjectHead client key opts = liftIO $
  (fmap.fmap)
    (map (() <$) . ObjectR.fromGetResp)
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
  => Client -- ^
  -> Key -- ^
  -> Vclock -- ^
  -> GetObjectOpts -- ^
  -> m (Result (IfModified [ObjectR ByteString]))
getObjectIfModified client key vclock opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Unmodified
        else Modified (ObjectR.fromGetResp response))
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
  => Client -- ^
  -> Key -- ^
  -> Vclock -- ^
  -> GetObjectOpts -- ^
  -> m (Result (IfModified [ObjectR ()]))
getObjectHeadIfModified client key vclock opts = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getObject (iface client) request)
  where
    request :: RpbGetReq
    request =
      makeRpbGetReq key opts
        & L.head .~ True
        & L.ifModified .~ unVclock vclock

    fromResponse :: RpbGetResp -> IfModified [ObjectR ()]
    fromResponse response =
      if response ^. L.unchanged
        then Unmodified
        else Modified ((() <$) <$> ObjectR.fromGetResp response)

makeRpbGetReq :: Key -> GetObjectOpts -> RpbGetReq
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


-- | Put an object.
putObject ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> ObjectW -- ^
  -> PutObjectOpts -- ^
  -> m (Result ())
putObject client (Key { type', bucket, key }) object opts = liftIO $
  (() <$) <$> Interface.putObject (iface client) request
  where
    request :: RpbPutReq
    request =
      makeRpbPutReq type' bucket (Just key) object opts

-- | Put an object and return its metadata.
--
-- If multiple siblings are returned, you should perform a 'getObject',
-- resolve them, then perform a 'putObject'.
putObjectHead ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> ObjectW -- ^
  -> PutObjectOpts -- ^
  -> m (Result (NonEmpty (ObjectR ())))
putObjectHead client (Key { type', bucket, key }) object opts = liftIO $
  (fmap.fmap)
    (fmap (() <$) . ObjectR.fromPutResp)
    (Interface.putObject (iface client) request)

  where
    request :: RpbPutReq
    request =
      makeRpbPutReq type' bucket (Just key) object opts
        & L.returnHead .~ True

-- | Put an object and return it.
--
-- If multiple siblings are returned, you should perform a 'getObject',
-- resolve them, then perform a 'putObject'.
putObjectBody ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> ObjectW -- ^
  -> PutObjectOpts -- ^
  -> m (Result (NonEmpty (ObjectR ByteString)))
putObjectBody client (Key { type', bucket, key }) object opts = liftIO $
  (fmap.fmap)
    ObjectR.fromPutResp
    (Interface.putObject (iface client) request)

  where
    request :: RpbPutReq
    request =
      makeRpbPutReq type' bucket (Just key) object opts
        & L.returnBody .~ True

-- | Put a new object and return its randomly-generated key.
putNewObject ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> ObjectW -- ^
  -> PutObjectOpts -- ^
  -> m (Result Key)
putNewObject client (Bucket { type', bucket }) object opts = liftIO $
  (fmap.fmap)
    (\response ->
      Key
        { type' = type'
        , bucket = bucket
        , key = response ^. L.key
        })
    (Interface.putObject (iface client) request)
  where
    request :: RpbPutReq
    request =
      makeRpbPutReq type' bucket Nothing object opts

-- | Put a new object and return its randomly-generated key and metadata.
putNewObjectHead ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> ObjectW -- ^
  -> PutObjectOpts -- ^
  -> m (Result (Key, ObjectR ()))
putNewObjectHead client (Bucket { type', bucket }) object opts = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.putObject (iface client) request)

  where
    request :: RpbPutReq
    request =
      makeRpbPutReq type' bucket Nothing object opts
        & L.returnHead .~ True

    fromResponse :: RpbPutResp -> (Key, ObjectR ())
    fromResponse response =
      (key, object)

      where
        key :: Key
        key =
          Key
            { type' = type'
            , bucket = bucket
            , key = response ^. L.key
            }

        object :: ObjectR ()
        object =
          () <$ List1.head (ObjectR.fromPutResp response)

makeRpbPutReq ::
     ByteString
  -> ByteString
  -> Maybe ByteString
  -> ObjectW
  -> PutObjectOpts
  -> RpbPutReq
makeRpbPutReq type' bucket key object opts =
  defMessage
    & L.bucket .~ bucket
    & L.content .~
        (defMessage
          & L.indexes .~ map Index.toPair (object ^. field @"indexes")
          & L.maybe'charset .~ (object ^. field @"charset")
          & L.maybe'contentEncoding .~ (object ^. field @"contentEncoding")
          & L.maybe'contentType .~ (object ^. field @"contentType")
          & L.usermeta .~ map Pair.fromTuple (object ^. field @"metadata")
          & L.value .~ (object ^. field @"value")
        )
    & L.maybe'dw .~ Quorum.toWord32 (dw opts)
    & L.maybe'key .~ key
    & L.maybe'nVal .~ Quorum.toWord32 (opts ^. field @"n")
    & L.maybe'pw .~ Quorum.toWord32 (pw opts)
    & L.maybe'vclock .~ coerce (object ^. field @"vclock")
    & L.maybe'w .~ Quorum.toWord32 (w opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")
    & L.maybe'sloppyQuorum .~ defFalse (opts ^. field @"sloppyQuorum")
    & L.type' .~ type'


defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True

defTrue :: Bool -> Maybe Bool
defTrue = \case
  False -> Just False
  True -> Nothing
