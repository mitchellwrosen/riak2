module Riak.Object
  ( -- * Object
    Object(..)
    -- ** Get object
  , get
  , getHead
  , getIfModified
  , getHeadIfModified
    -- ** Put object
  , put
  , putGet
  , putGetHead
    -- ** Delete object
  , delete
  ) where

import Riak.Content          (Content(..))
import Riak.Internal.Client  (Client, Error)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Object  (Object(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Opts             (GetOpts(..), PutOpts(..))
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.Internal.Client         as Client
import qualified Riak.Internal.Object         as Object
import qualified Riak.Internal.Proto.Pair     as Proto.Pair
import qualified Riak.Internal.Quorum         as Quorum
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex
import qualified Riak.Proto                   as Proto
import qualified Riak.Proto.Lens              as L

import Data.Generics.Product.Typed (HasType(..))

import qualified Data.ByteString as ByteString


-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
get
  :: MonadIO m
  => Client -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either Error [Object ByteString])
get client key opts = liftIO $
  (fmap.fmap)
    (Object.fromGetResponse key)
    (doGet client request)

  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts

-- | Get an object's metadata.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
getHead
  :: MonadIO m
  => Client -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either Error [Object ()])
getHead client key opts = liftIO $
  (fmap.fmap)
    (map (() <$) . Object.fromGetResponse key)
    (doGet client request)
  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts
        & L.head .~ True

-- | Get an object if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
getIfModified
  :: MonadIO m
  => Client -- ^
  -> Content a -- ^
  -> GetOpts -- ^
  -> m (Either Error (Maybe [Object ByteString]))
getIfModified client (Content { key, context }) opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Nothing
        else Just (Object.fromGetResponse key response))
    (doGet client request)

  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts
        & L.ifModified .~ unContext context

-- | Get an object's metadata if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
getHeadIfModified
  :: MonadIO m
  => Client -- ^
  -> Content a -- ^
  -> GetOpts -- ^
  -> m (Either Error (Maybe [Object ()]))
getHeadIfModified client (Content { key, context }) opts = liftIO $
  (fmap.fmap)
    fromResponse
    (doGet client request)
  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts
        & L.head .~ True
        & L.ifModified .~ unContext context

    fromResponse :: Proto.GetResponse -> Maybe [Object ()]
    fromResponse response =
      if response ^. L.unchanged
        then Nothing
        else Just ((() <$) <$> Object.fromGetResponse key response)

doGet ::
     Client
  -> Proto.GetRequest
  -> IO (Either Error Proto.GetResponse)
doGet client request =
  Client.exchange
    client
    (RequestGet request)
    (\case
      ResponseGet response -> Just response
      _ -> Nothing)

makeGetRequest :: Key -> GetOpts -> Proto.GetRequest
makeGetRequest key opts =
  defMessage
    & L.bucket .~ (key ^. field @"bucket")
    & L.bucketType .~ key ^. field @"bucketType"
    & L.deletedContext .~ True
    & L.head .~ True
    & L.key .~ (key ^. field @"key")
    & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
    & L.maybe'n .~ Quorum.toWord32 (opts ^. field @"n")
    & L.maybe'notfoundOk .~ defTrue (notfoundOk opts)
    & L.maybe'pr .~ Quorum.toWord32 (pr opts)
    & L.maybe'r .~ Quorum.toWord32 (r opts)
    & L.maybe'sloppyQuorum .~ defFalse (opts ^. field @"sloppyQuorum")
    & L.maybe'timeout .~ (opts ^. field @"timeout")


-- | Put an object and return its key.
--
-- /See also/: Riak.Key.'Riak.Key.none'
put ::
     ( HasType (Content ByteString) content
     , MonadIO m
     )
  => Client -- ^
  -> content -- ^
  -> PutOpts -- ^
  -> m (Either Error Key)
put client content opts =
  liftIO (put_ client (content ^. typed) opts)

put_ ::
     Client
  -> Content ByteString
  -> PutOpts
  -> IO (Either Error Key)
put_ client content opts =
  (fmap.fmap)
    fromResponse
    (doPut client request)
  where
    request :: Proto.PutRequest
    request =
      makePutRequest key content opts

    key@(Key _ _ k) =
      content ^. field @"key"

    fromResponse :: Proto.PutResponse -> Key
    fromResponse response =
      if ByteString.null k
        then key { key = response ^. L.key }
        else key

-- | Put an object and return it.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
--
-- /See also/: Riak.Key.'Riak.Key.none'
putGet ::
     ( HasType (Content ByteString) content
     , MonadIO m
     )
  => Client -- ^
  -> content -- ^
  -> PutOpts -- ^
  -> m (Either Error (NonEmpty (Object ByteString)))
putGet client content opts =
  liftIO (putGet_ client (content ^. typed) opts)

putGet_ ::
     Client -- ^
  -> Content ByteString -- ^
  -> PutOpts -- ^
  -> IO (Either Error (NonEmpty (Object ByteString)))
putGet_ client content opts =
  (fmap.fmap)
    (Object.fromPutResponse key)
    (doPut client request)

  where
    request :: Proto.PutRequest
    request =
      makePutRequest key content opts
        & L.returnBody .~ True

    key :: Key
    key =
      content ^. field @"key"

-- | Put an object and return its metadata.
--
-- If multiple siblings are returned, you should perform a 'get', resolve them,
-- then perform a 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Metadata.deleted'.
--
-- /See also/: Riak.Key.'Riak.Key.none'
putGetHead ::
     ( HasType (Content ByteString) content
     , MonadIO m
     )
  => Client -- ^
  -> content -- ^
  -> PutOpts -- ^
  -> m (Either Error (NonEmpty (Object ())))
putGetHead client content opts =
  liftIO (putGetHead_ client (content ^. typed) opts)

putGetHead_ ::
     Client
  -> Content ByteString
  -> PutOpts
  -> IO (Either Error (NonEmpty (Object ())))
putGetHead_ client content opts =
  (fmap.fmap)
    (fmap (() <$) . Object.fromPutResponse key)
    (doPut client request)

  where
    request :: Proto.PutRequest
    request =
      makePutRequest key content opts
        & L.returnHead .~ True

    key :: Key
    key =
      content ^. field @"key"

doPut ::
     Client
  -> Proto.PutRequest
  -> IO (Either Error Proto.PutResponse)
doPut client request =
  Client.exchange
    client
    (RequestPut request)
    (\case
      ResponsePut response -> Just response
      _ -> Nothing)

makePutRequest ::
     Key
  -> Content ByteString
  -> PutOpts
  -> Proto.PutRequest
makePutRequest (Key bucketType bucket key) content opts =
  defMessage
    & L.bucket .~ bucket
    & L.bucketType .~ bucketType
    & L.content .~
        (defMessage
          & L.indexes .~ map SecondaryIndex.toPair (content ^. field @"indexes")
          & L.maybe'charset .~ (content ^. field @"charset")
          & L.maybe'contentEncoding .~ (content ^. field @"encoding")
          & L.maybe'contentType .~ (content ^. field @"type'")
          & L.usermeta .~ map Proto.Pair.fromTuple (content ^. field @"metadata")
          & L.value .~ (content ^. field @"value")
        )
    & L.maybe'dw .~ Quorum.toWord32 (dw opts)
    & L.maybe'key .~
        (if ByteString.null key
          then Nothing
          else Just key)
    & L.maybe'n .~ Quorum.toWord32 (opts ^. field @"n")
    & L.maybe'pw .~ Quorum.toWord32 (pw opts)
    & L.maybe'context .~
        (let
          context :: ByteString
          context =
            unContext (content ^. field @"context")
        in
          if ByteString.null context
            then Nothing
            else Just context)
    & L.maybe'w .~ Quorum.toWord32 (w opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")
    & L.maybe'sloppyQuorum .~ defFalse (opts ^. field @"sloppyQuorum")

delete ::
     MonadIO m
  => Client -- ^
  -> Object a -- ^
  -> m (Either Error ())
delete client (view (field @"content") -> content) = liftIO $
  Client.exchange
    client
    (RequestDelete request)
    (\case
      ResponseDelete{} -> Just ()
      _ -> Nothing)

  where
    request :: Proto.DeleteRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.key .~ key
        -- TODO delete opts
        -- & L.maybe'dw .~ undefined
        -- & L.maybe'n .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'pw .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'rw .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined
        -- & L.maybe'w .~ undefined
        & L.context .~ unContext (content ^. field @"context")

    Key bucketType bucket key =
      content ^. field @"key"

defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True

defTrue :: Bool -> Maybe Bool
defTrue = \case
  False -> Just False
  True -> Nothing
