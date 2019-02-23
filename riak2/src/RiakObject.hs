-- TODO timeout variants since Riak will only return {error, timeout} if one was
-- requested?

module RiakObject where

import Libriak.Handle (Handle)
import RiakContent    (Content(..))
import RiakContext    (Context(..), newContext)
import RiakError
import RiakKey        (Key(..))
import RiakQuorum     (Quorum)
import RiakSibling    (Sibling(..))
import RiakUtils      (retrying)

import qualified Libriak.Handle     as Handle
import qualified Libriak.Proto      as Proto
import qualified RiakKey            as Key
import qualified RiakProtoContent   as Proto.Content
import qualified RiakQuorum         as Quorum
import qualified RiakSecondaryIndex as SecondaryIndex
import qualified RiakSibling        as Sibling


import Control.Lens          ((.~), (^.))
import Data.Default.Class    (Default(..))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)


import qualified Data.ByteString    as ByteString
import qualified Data.List.NonEmpty as List1



data Object a
  = Object
  { content :: !a
  , context :: !Context -- ^ Causal context
  , key :: !Key -- ^ Key
  } deriving stock (Eq, Functor, Generic, Show)

-- TODO better names for pr/r
-- TODO basicQuorum/notfoundOk -> NotfoundBehavior
data GetOpts
  = GetOpts
  { basicQuorum :: !(Maybe Bool)
  , nodes :: !(Maybe Natural)
  , notfoundOk :: !(Maybe Bool)
  , pr :: !(Maybe Quorum)
  , r :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { basicQuorum = Nothing
      , nodes = Nothing
      , notfoundOk = Nothing
      , pr = Nothing
      , r = Nothing
      , timeout = Nothing
      }

data PutOpts
  = PutOpts
  { dw :: !(Maybe Quorum)
  , nodes :: !(Maybe Natural)
  , pw :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  , w :: !(Maybe Quorum)
  } deriving stock (Generic, Show)

instance Default PutOpts where
  def :: PutOpts
  def =
    PutOpts
      { dw = Nothing
      , nodes = Nothing
      , pw = Nothing
      , timeout = Nothing
      , w = Nothing
      }

data DeleteOpts
  = DeleteOpts
  { dw :: !(Maybe Quorum)
  , nodes :: !(Maybe Quorum)
  , pr :: !(Maybe Quorum)
  , pw :: !(Maybe Quorum)
  , r :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32)
  , w :: !(Maybe Quorum)
  } deriving stock (Generic, Show)

instance Default DeleteOpts where
  def :: DeleteOpts
  def =
    DeleteOpts
      { dw = Nothing
      , nodes = Nothing
      , pr = Nothing
      , pw = Nothing
      , r = Nothing
      , timeout = Nothing
      , w = Nothing
      }


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
  | isOverloadError err =
      Just OverloadError
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

makeGetRequest :: Key -> GetOpts -> Proto.RpbGetReq
makeGetRequest
    key
    GetOpts { basicQuorum, nodes, notfoundOk, pr, r, timeout } =

  Proto.defMessage
    & Key.setProto key
    & Proto.deletedvclock .~ True
    & Proto.maybe'basicQuorum .~ basicQuorum
    & Proto.maybe'notfoundOk .~ notfoundOk
    & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
    & Proto.maybe'pr .~ (Quorum.toWord32 <$> pr)
    & Proto.maybe'r .~ (Quorum.toWord32 <$> r)
    & Proto.maybe'timeout .~ timeout


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
doPut_ handle request =
  Handle.put handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parsePutError request err)

    Right (Right response) ->
      pure (Just (Right response))

parsePutError :: Proto.RpbPutReq -> ByteString -> Maybe PutError
parsePutError request err
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
     PutOpts { dw, nodes, pw, timeout, w } =
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
    & Proto.maybe'dw .~ (Quorum.toWord32 <$> dw)
    & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
    & Proto.maybe'pw .~ (Quorum.toWord32 <$> pw)
    & Proto.maybe'w .~ (Quorum.toWord32 <$> w)
    & Proto.maybe'timeout .~ timeout
    & Proto.maybe'vclock .~
        (if ByteString.null (unContext context)
          then Nothing
          else Just (unContext context))

-- | Delete an object.
delete ::
     MonadIO m
  => Handle -- ^
  -> Object a -- ^
  -> DeleteOpts -- ^
  -> m (Either DeleteError ())
delete handle object opts =
  liftIO (retrying 1000000 (delete_ handle object opts))

delete_ ::
     Handle
  -> Object a
  -> DeleteOpts
  -> IO (Maybe (Either DeleteError ()))
delete_
    handle
    Object { context, key }
    DeleteOpts { dw, nodes, pr, pw, r, timeout, w } =

  Handle.delete handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseDeleteError err)

    Right (Right response) ->
      pure (Just (Right response))

  where
    request :: Proto.RpbDelReq
    request =
      Proto.defMessage
        & Key.setProto key
        & Proto.maybe'dw .~ (Quorum.toWord32 <$> dw)
        & Proto.maybe'nVal .~ (Quorum.toWord32 <$> nodes)
        & Proto.maybe'pr .~ (Quorum.toWord32 <$> pr)
        & Proto.maybe'pw .~ (Quorum.toWord32 <$> pw)
        & Proto.maybe'r .~ (Quorum.toWord32 <$> r)
        & Proto.maybe'timeout .~ timeout
        & Proto.maybe'w .~ (Quorum.toWord32 <$> w)
        & Proto.vclock .~ unContext context

parseDeleteError :: ByteString -> Maybe DeleteError
parseDeleteError err
  | isOverloadError err =
      Just OverloadError
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))
