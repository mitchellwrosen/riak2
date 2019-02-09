module Riak.Object
  ( -- * Object operations
    -- ** Get object
    get
  , getHead
  , getIfModified
  , getHeadIfModified
    -- ** Put object
  , put
  , putGet
  , putGetHead
    -- ** Delete object
  , delete
    -- * Object type
  , Object(..)
  ) where

import Riak.Content          (Content(..))
import Riak.Handle           (Handle)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Error
import Riak.Internal.Object  (Object(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Opts             (GetOpts(..), PutOpts(..))

import qualified Riak.Handle                  as Handle
import qualified Riak.Internal.Object         as Object
import qualified Riak.Internal.Proto.Pair     as Proto.Pair
import qualified Riak.Internal.Quorum         as Quorum
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex
import qualified Riak.Proto                   as Proto
import qualified Riak.Proto.Lens              as L

import Control.Lens                ((.~), (^.))
import Data.Generics.Product       (field)
import Data.Generics.Product.Typed (HasType(..))
import Data.Text.Encoding          (decodeUtf8)

import qualified ByteString

-- TODO specialize HasType for Object,Content


-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Object.deleted'.
get
  :: MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) [Object ByteString])
get handle key opts = liftIO $
  (fmap.fmap)
    (Object.fromGetResponse key)
    (doGet handle request)

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
-- 'Riak.Object.deleted'.
getHead
  :: MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) [Object ()])
getHead handle key opts = liftIO $
  (fmap.fmap)
    (map (() <$) . Object.fromGetResponse key)
    (doGet handle request)
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
-- 'Riak.Object.deleted'.
getIfModified ::
     ∀ a object m.
     ( HasType (Content a) (object a)
     , MonadIO m
     )
  => Handle -- ^
  -> object a -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Maybe [Object ByteString]))
getIfModified handle object opts =
  liftIO (getIfModified_ handle (object ^. typed @(Content a)) opts)

getIfModified_ ::
     Handle
  -> Content a
  -> GetOpts
  -> IO (Either (Error 'GetOp) (Maybe [Object ByteString]))
getIfModified_ handle (Content { key, context }) opts =
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Nothing
        else Just (Object.fromGetResponse key response))
    (doGet handle request)

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
-- 'Riak.Object.deleted'.
getHeadIfModified ::
     ∀ a object m.
     ( HasType (Content a) (object a)
     , MonadIO m
     )
  => Handle -- ^
  -> object a -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Maybe [Object ()]))
getHeadIfModified handle object opts =
  liftIO (getHeadIfModified_ handle (object ^. typed @(Content a)) opts)

getHeadIfModified_ ::
     Handle
  -> Content a
  -> GetOpts
  -> IO (Either (Error 'GetOp) (Maybe [Object ()]))
getHeadIfModified_ handle (Content { key, context }) opts =
  (fmap.fmap)
    fromResponse
    (doGet handle request)
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
     Handle
  -> Proto.GetRequest
  -> IO (Either (Error 'GetOp) Proto.GetResponse)
doGet handle request =
  first parseGetError <$>
    Handle.get handle request
  where
    parseGetError :: Handle.Error -> Error 'GetOp
    parseGetError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isBucketTypeDoesNotExistError err ->
            BucketTypeDoesNotExistError (request ^. L.bucketType)
        | isInvalidNError err ->
            InvalidNError (request ^. L.n)
        | otherwise ->
            UnknownError (decodeUtf8 err)

makeGetRequest :: Key -> GetOpts -> Proto.GetRequest
makeGetRequest (Key bucketType bucket key) opts =
  Proto.defMessage
    & L.bucket .~ bucket
    & L.bucketType .~ bucketType
    & L.deletedContext .~ True
    & L.key .~ key
    & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
    & L.maybe'n .~ (Quorum.toWord32 <$> (opts ^. field @"n"))
    & L.maybe'notfoundOk .~ notfoundOk opts
    & L.maybe'pr .~ (Quorum.toWord32 <$> pr opts)
    & L.maybe'r .~ (Quorum.toWord32 <$> r opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")


-- | Put an object and return its key.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
put ::
     ( HasType (Content ByteString) (object ByteString)
     , MonadIO m
     )
  => Handle -- ^
  -> object ByteString -- ^
  -> PutOpts -- ^
  -> m (Either (Error 'PutOp) Key)
put handle object opts =
  liftIO (put_ handle (object ^. typed) opts)

put_ ::
     Handle
  -> Content ByteString
  -> PutOpts
  -> IO (Either (Error 'PutOp) Key)
put_ handle content opts =
  (fmap.fmap)
    fromResponse
    (doPut handle request)
  where
    request :: Proto.PutRequest
    request =
      makePutRequest key content opts

    key@(Key bucketType bucket k) =
      content ^. field @"key"

    fromResponse :: Proto.PutResponse -> Key
    fromResponse response =
      if ByteString.null k
        then Key bucketType bucket (response ^. L.key)
        else key

-- | Put an object and return it.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
--
-- /Note/: The object(s) returned may be tombstones; check
-- 'Riak.Object.deleted'.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
putGet ::
     ( HasType (Content ByteString) (object ByteString)
     , MonadIO m
     )
  => Handle -- ^
  -> object ByteString -- ^
  -> PutOpts -- ^
  -> m (Either (Error 'PutOp) (NonEmpty (Object ByteString)))
putGet handle object opts =
  liftIO (putGet_ handle (object ^. typed) opts)

putGet_ ::
     Handle -- ^
  -> Content ByteString -- ^
  -> PutOpts -- ^
  -> IO (Either (Error 'PutOp) (NonEmpty (Object ByteString)))
putGet_ handle content opts =
  (fmap.fmap)
    (Object.fromPutResponse key)
    (doPut handle request)

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
-- 'Riak.Object.deleted'.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
putGetHead ::
     ( HasType (Content ByteString) (object ByteString)
     , MonadIO m
     )
  => Handle -- ^
  -> object ByteString -- ^
  -> PutOpts -- ^
  -> m (Either (Error 'PutOp) (NonEmpty (Object ())))
putGetHead handle object opts =
  liftIO (putGetHead_ handle (object ^. typed) opts)

putGetHead_ ::
     Handle
  -> Content ByteString
  -> PutOpts
  -> IO (Either (Error 'PutOp) (NonEmpty (Object ())))
putGetHead_ handle content opts =
  (fmap.fmap)
    (fmap (() <$) . Object.fromPutResponse key)
    (doPut handle request)

  where
    request :: Proto.PutRequest
    request =
      makePutRequest key content opts
        & L.returnHead .~ True

    key :: Key
    key =
      content ^. field @"key"

doPut ::
     Handle
  -> Proto.PutRequest
  -> IO (Either (Error 'PutOp) Proto.PutResponse)
doPut handle request =
  first parsePutError <$> Handle.put handle request

  where
    parsePutError :: Handle.Error -> Error 'PutOp
    parsePutError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isBucketTypeDoesNotExistError err ->
            BucketTypeDoesNotExistError (request ^. L.bucketType)
        | isInvalidNError err ->
            InvalidNError (request ^. L.n)
        | otherwise ->
            UnknownError (decodeUtf8 err)

makePutRequest ::
     Key
  -> Content ByteString
  -> PutOpts
  -> Proto.PutRequest
makePutRequest (Key bucketType bucket key) content opts =
  Proto.defMessage
    & L.bucket .~ bucket
    & L.bucketType .~ bucketType
    & L.content .~
        (Proto.defMessage
          & L.indexes .~ map SecondaryIndex.toPair (content ^. field @"indexes")
          & L.maybe'charset .~ (content ^. field @"charset")
          & L.maybe'contentEncoding .~ (content ^. field @"encoding")
          & L.maybe'contentType .~ (content ^. field @"type'")
          & L.usermeta .~ map Proto.Pair.fromTuple (content ^. field @"metadata")
          & L.value .~ (content ^. field @"value")
        )
    & L.maybe'dw .~ (Quorum.toWord32 <$> dw opts)
    & L.maybe'key .~
        (if ByteString.null key
          then Nothing
          else Just key)
    & L.maybe'n .~ (Quorum.toWord32 <$> (opts ^. field @"n"))
    & L.maybe'pw .~ (Quorum.toWord32 <$> pw opts)
    & L.maybe'context .~
        (let
          context :: ByteString
          context =
            unContext (content ^. field @"context")
        in
          if ByteString.null context
            then Nothing
            else Just context)
    & L.maybe'w .~ (Quorum.toWord32 <$> w opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")

-- | Delete an object.
delete ::
     ∀ a object m.
     ( HasType (Content a) (object a)
     , MonadIO m
     )
  => Handle -- ^
  -> object a -- ^
  -> m (Either (Error 'DeleteOp) ())
delete handle object =
  liftIO (delete_ handle (object ^. typed @(Content a)))

delete_ ::
     Handle -- ^
  -> Content a -- ^
  -> IO (Either (Error 'DeleteOp) ())
delete_ handle content =
  first parseDeleteError <$> Handle.delete handle request

  where
    request :: Proto.DeleteRequest
    request =
      Proto.defMessage
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
        -- & L.maybe'timeout .~ undefined
        -- & L.maybe'w .~ undefined
        & L.context .~ unContext (content ^. field @"context")

    Key bucketType bucket key =
      content ^. field @"key"

    parseDeleteError :: Handle.Error -> Error 'DeleteOp
    parseDeleteError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err ->
        UnknownError (decodeUtf8 err)

defFalse :: Bool -> Maybe Bool
defFalse = \case
  False -> Nothing
  True -> Just True
