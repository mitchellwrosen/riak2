-- TODO timeout variants since Riak will only return {error, timeout} if one was
-- requested?
--
-- TODO add *With variants

module RiakObject where

import RiakContent (Content(..))
import RiakContext (Context(..), newContext)
import RiakError
import RiakGetOpts (GetOpts)
import RiakHandle  (Handle)
import RiakKey     (Key(..))
import RiakPutOpts (PutOpts)
import RiakSibling (Sibling(..))
import RiakUtils   (retrying)

import qualified RiakBucket         as Bucket
import qualified RiakGetOpts        as GetOpts
import qualified RiakHandle         as Handle
import qualified RiakKey            as Key
import qualified RiakProtoContent   as Proto.Content
import qualified RiakPutOpts        as PutOpts
import qualified RiakSecondaryIndex as SecondaryIndex
import qualified RiakSibling        as Sibling

import Control.Lens          ((.~), (^.))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)

import qualified Data.ByteString    as ByteString
import qualified Data.List.NonEmpty as List1
import qualified Data.Riak.Proto    as Proto


data Object a
  = Object
  { content :: a
  , context :: Context -- ^ Causal context
  , key :: Key -- ^ Key
  } deriving stock (Eq, Functor, Generic, Show)


newObject ::
     Key -- ^ Key
  -> a -- ^ Content
  -> Object a
newObject key content =
  Object
    { content = content
    , context = newContext
    , key = key
    }

-- | Parse an object from a get response.
fromGetResponse ::
     Key
  -> Proto.RpbGetResp
  -> Object [Sibling ByteString]
fromGetResponse key response =
  Object
    { content = map Sibling.fromProtoContent (response ^. Proto.content)
    , context = Context (response ^. Proto.vclock)
    , key = key
    }

-- | Parse an object from a put response.
--
-- Assumes that either @return_body@ or @return_head@ was set on the request.
fromPutResponse ::
     Key
  -> Proto.RpbPutResp
  -> Object (NonEmpty (Sibling ByteString))
fromPutResponse k@(Key bucketType bucket key) response =
  Object
    { content = List1.fromList (map Sibling.fromProtoContent (response ^. Proto.content))
    , context = Context (response ^. Proto.vclock)
    , key = key'
    }

  where
    key' :: Key
    key' =
      if ByteString.null key
        then Key bucketType bucket (response ^. Proto.key)
        else k


-- | Get an object.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
get ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetError (Object [Sibling ByteString]))
get handle key opts = liftIO $
  (fmap.fmap)
    (fromGetResponse key)
    (doGet handle request)

  where
    request :: Proto.RpbGetReq
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
  -> m (Either GetError (Object [Sibling ()]))
getHead handle key opts = liftIO $
  (fmap.fmap)
    ((fmap.map) (() <$)  . fromGetResponse key)
    (doGet handle request)
  where
    request :: Proto.RpbGetReq
    request =
      makeGetRequest key opts
        & Proto.head .~ True

-- | Get an object if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
getIfModified ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> GetOpts -- ^
  -> m (Either GetError (Maybe (Object [Sibling ByteString])))
getIfModified handle Object { context, key } opts = liftIO $
  (fmap.fmap)
    (\response ->
      if response ^. Proto.unchanged
        then Nothing
        else Just (fromGetResponse key response))
    (doGet handle request)

  where
    request :: Proto.RpbGetReq
    request =
      makeGetRequest key opts
        & Proto.ifModified .~ unContext context

-- | Get an object's metadata if it has been modified since the given version.
--
-- If multiple siblings are returned, you should resolve them, then perform a
-- 'put'.
getHeadIfModified ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> GetOpts -- ^
  -> m (Either GetError (Maybe (Object [Sibling ()])))
getHeadIfModified handle Object { context, key } opts = liftIO $
  (fmap.fmap)
    fromResponse
    (doGet handle request)
  where
    request :: Proto.RpbGetReq
    request =
      makeGetRequest key opts
        & Proto.head .~ True
        & Proto.ifModified .~ unContext context

    fromResponse :: Proto.RpbGetResp -> Maybe (Object [Sibling ()])
    fromResponse response =
      if response ^. Proto.unchanged
        then Nothing
        else Just (map (() <$) <$> fromGetResponse key response)

doGet ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Either GetError Proto.RpbGetResp)
doGet handle request =
  retrying 1000000 (doGet_ handle request)

doGet_ ::
     Handle
  -> Proto.RpbGetReq
  -> IO (Maybe (Either GetError Proto.RpbGetResp))
doGet_ handle request =
  Handle.get handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseGetError request err)

    Right (Right response) ->
      pure (Just (Right response))

parseGetError :: Proto.RpbGetReq -> ByteString -> Maybe GetError
parseGetError request err
  | isBucketTypeDoesNotExistError0 err =
      Just (BucketTypeDoesNotExistError (request ^. Proto.type'))
  | isInvalidNodesError0 err =
      Just InvalidNodesError
  | isKeyCannotBeZeroLengthError err =
      Just (InvalidKeyError (Key.fromProto request))
  | isOverloadError err =
      Just OverloadError
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

makeGetRequest :: Key -> GetOpts -> Proto.RpbGetReq
makeGetRequest key opts =
  Proto.defMessage
    & GetOpts.setProto opts
    & Key.setProto key
    & Proto.deletedvclock .~ True


-- | Put an object and return its key.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
put ::
     Handle -- ^
  -> Object (Content ByteString) -- ^
  -> PutOpts -- ^
  -> IO (Either PutError Key)
put handle object opts =
  (fmap.fmap)
    fromResponse
    (doPut handle request)
  where
    request :: Proto.RpbPutReq
    request =
      makePutRequest object opts

    key@(Key bucketType bucket k) =
      object ^. field @"key"

    fromResponse :: Proto.RpbPutResp -> Key
    fromResponse response =
      if ByteString.null k
        then Key bucketType bucket (response ^. Proto.key)
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
  -> m (Either PutError (Object (NonEmpty (Sibling ByteString))))
putGet handle object opts = liftIO $
  (fmap.fmap)
    (fromPutResponse (object ^. field @"key"))
    (doPut handle request)

  where
    request :: Proto.RpbPutReq
    request =
      makePutRequest object opts
        & Proto.returnBody .~ True

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
  -> m (Either PutError (Object (NonEmpty (Sibling ()))))
putGetHead handle object opts = liftIO $
  (fmap.fmap)
    ((fmap.fmap) (() <$) . fromPutResponse (object ^. field @"key"))
    (doPut handle request)

  where
    request :: Proto.RpbPutReq
    request =
      makePutRequest object opts
        & Proto.returnHead .~ True

doPut ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Either PutError Proto.RpbPutResp)
doPut handle request =
  retrying 1000000 (doPut_ handle request)

doPut_ ::
     Handle
  -> Proto.RpbPutReq
  -> IO (Maybe (Either PutError Proto.RpbPutResp))
doPut_ handle request = do
  Handle.put handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parsePutError request err)

    Right (Right response) ->
      pure (Just (Right response))

parsePutError :: Proto.RpbPutReq -> ByteString -> Maybe PutError
parsePutError request err
  | isBucketCannotBeZeroLengthError err =
      Just (InvalidBucketError (Bucket.fromProto request))
  | isBucketTypeDoesNotExistError0 err =
      Just (BucketTypeDoesNotExistError (request ^. Proto.type'))
  | isInvalidNodesError0 err =
      Just InvalidNodesError
  | isOverloadError err =
      Just OverloadError
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

makePutRequest ::
     Object (Content ByteString)
  -> PutOpts
  -> Proto.RpbPutReq
makePutRequest
     Object { content = Content { charset, encoding, indexes, metadata, type', value }, context, key }
     opts =
  Proto.defMessage
    & Key.setMaybeProto key
    & Proto.content .~
        (Proto.defMessage
          & Proto.Content.setMetadata metadata
          & Proto.indexes .~ map SecondaryIndex.toPair indexes
          & Proto.maybe'charset .~ charset
          & Proto.maybe'contentEncoding .~ encoding
          & Proto.maybe'contentType .~ type'
          & Proto.value .~ value
        )
    & Proto.maybe'vclock .~
        (if ByteString.null (unContext context)
          then Nothing
          else Just (unContext context))
    & PutOpts.setProto opts

-- | Delete an object.
delete ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> PutOpts -- ^
  -> m (Either PutError ())
delete handle Object { context, key } opts = liftIO $
  (() <$) <$> doPut handle request

  where
    request :: Proto.RpbPutReq
    request =
      Proto.defMessage
        & Key.setMaybeProto key
        & Proto.content .~
            (Proto.defMessage
              & Proto.deleted .~ True)
        & Proto.maybe'vclock .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & PutOpts.setProto opts
