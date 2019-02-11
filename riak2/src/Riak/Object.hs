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
  , newObject
  ) where

import Libriak.Handle        (Handle)
import Riak.Content          (Content(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Error
import Riak.Internal.Key     (Key(..))
import Riak.Internal.Object  (Object(..), fromGetResponse, fromPutResponse,
                              newObject)
import Riak.Internal.Prelude
import Riak.Internal.Sibling (Sibling)
import Riak.Opts             (GetOpts(..), PutOpts(..))

import qualified Libriak.Handle               as Handle
import qualified Libriak.Proto                as Proto
import qualified Libriak.Proto.Lens           as L
import qualified Riak.Internal.Key            as Key
import qualified Riak.Internal.Proto.Pair     as Proto.Pair
import qualified Riak.Internal.Quorum         as Quorum
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex

import Control.Lens          ((.~), (^.))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)

import qualified ByteString

-- TODO specialize HasType for Object,Content


-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
get ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Object [Sibling ByteString]))
get handle key opts = liftIO $
  (fmap.fmap)
    (fromGetResponse key)
    (doGet handle request)

  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts

-- | Get an object's metadata.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
getHead ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Object [Sibling ()]))
getHead handle key opts = liftIO $
  (fmap.fmap)
    ((fmap.map) (() <$)  . fromGetResponse key)
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
getIfModified ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Maybe (Object [Sibling ByteString])))
getIfModified handle Object { context, key } opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. L.unchanged
        then Nothing
        else Just (fromGetResponse key response))
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
getHeadIfModified ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> GetOpts -- ^
  -> m (Either (Error 'GetOp) (Maybe (Object [Sibling ()])))
getHeadIfModified handle Object { context, key } opts = liftIO $
  (fmap.fmap)
    fromResponse
    (doGet handle request)
  where
    request :: Proto.GetRequest
    request =
      makeGetRequest key opts
        & L.head .~ True
        & L.ifModified .~ unContext context

    fromResponse :: Proto.GetResponse -> Maybe (Object [Sibling ()])
    fromResponse response =
      if response ^. L.unchanged
        then Nothing
        else Just (map (() <$) <$> fromGetResponse key response)

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
makeGetRequest key opts =
  Proto.defMessage
    & Key.setProto key
    & L.deletedContext .~ True
    & L.maybe'basicQuorum .~ defFalse (basicQuorum opts)
    & L.maybe'n .~ (Quorum.toWord32 <$> (opts ^. field @"n"))
    & L.maybe'notfoundOk .~ notfoundOk opts
    & L.maybe'pr .~ (Quorum.toWord32 <$> pr opts)
    & L.maybe'r .~ (Quorum.toWord32 <$> r opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")


-- | Put an object and return its key.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
put ::
     Handle -- ^
  -> Object (Content ByteString) -- ^
  -> PutOpts -- ^
  -> IO (Either (Error 'PutOp) Key)
put handle object opts =
  (fmap.fmap)
    fromResponse
    (doPut handle request)
  where
    request :: Proto.PutRequest
    request =
      makePutRequest object opts

    key@(Key bucketType bucket k) =
      object ^. field @"key"

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
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
putGet ::
     MonadIO m
  => Handle -- ^
  -> Object (Content ByteString) -- ^
  -> PutOpts -- ^
  -> m (Either (Error 'PutOp) (Object (NonEmpty (Sibling ByteString))))
putGet handle object opts = liftIO $
  (fmap.fmap)
    (fromPutResponse (object ^. field @"key"))
    (doPut handle request)

  where
    request :: Proto.PutRequest
    request =
      makePutRequest object opts
        & L.returnBody .~ True

-- | Put an object and return its metadata.
--
-- If multiple siblings are returned, you should perform a 'get', resolve them,
-- then perform a 'put'.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
putGetHead ::
     MonadIO m
  => Handle -- ^
  -> Object (Content ByteString) -- ^
  -> PutOpts -- ^
  -> m (Either (Error 'PutOp) (Object (NonEmpty (Sibling ()))))
putGetHead handle object opts = liftIO $
  (fmap.fmap)
    ((fmap.fmap) (() <$) . fromPutResponse (object ^. field @"key"))
    (doPut handle request)

  where
    request :: Proto.PutRequest
    request =
      makePutRequest object opts
        & L.returnHead .~ True

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
     Object (Content ByteString)
  -> PutOpts
  -> Proto.PutRequest
makePutRequest Object { content, context, key } opts =
  Proto.defMessage
    & Key.setMaybeProto key
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
    & L.maybe'n .~ (Quorum.toWord32 <$> (opts ^. field @"n"))
    & L.maybe'pw .~ (Quorum.toWord32 <$> pw opts)
    & L.maybe'context .~
        (if ByteString.null (unContext context)
          then Nothing
          else Just (unContext context))
    & L.maybe'w .~ (Quorum.toWord32 <$> w opts)
    & L.maybe'timeout .~ (opts ^. field @"timeout")

-- | Delete an object.
delete ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> m (Either (Error 'DeleteOp) ())
delete handle Object { context, key } = liftIO $
  first parseDeleteError <$> Handle.delete handle request

  where
    request :: Proto.DeleteRequest
    request =
      Proto.defMessage
        & Key.setProto key
        -- TODO delete opts
        -- & L.maybe'dw .~ undefined
        -- & L.maybe'n .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'pw .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'rw .~ undefined
        -- & L.maybe'timeout .~ undefined
        -- & L.maybe'w .~ undefined
        & L.context .~ unContext context

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
