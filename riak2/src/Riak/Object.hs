-- TODO timeout variants since Riak will only return {error, timeout} if one was
-- requested?

module Riak.Object
  ( -- * Object operations
    get
  , put
  , delete
    -- ** Get variants
  , getHead
  , getIfModified
  , getHeadIfModified
    -- ** Put variants
  , putGet
  , putGetHead
    -- * Object
  , Object(..)
  , newObject
    -- * Options
  , GetOpts(..)
  , PutOpts(..)
  , DeleteOpts(..)
  ) where

import Libriak.Handle        (Handle)
import Riak.Content          (Content(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Error
import Riak.Internal.Key     (Key(..))
import Riak.Internal.Object  (Object(..), fromGetResponse, fromPutResponse,
                              newObject)
import Riak.Internal.Prelude
import Riak.Internal.Quorum  (Quorum)
import Riak.Internal.Sibling (Sibling)

import qualified Libriak.Handle               as Handle
import qualified Libriak.Proto                as Proto
import qualified Riak.Internal.Key            as Key
import qualified Riak.Internal.Proto.Content  as Proto.Content
import qualified Riak.Internal.Quorum         as Quorum
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex

import Control.Lens          ((.~), (^.))
import Data.Default.Class    (Default(..))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)

import qualified ByteString


-- TODO better names for pr/r
-- TODO basicQuorum/notfoundOk -> NotfoundBehavior
data GetOpts
  = GetOpts
  { basicQuorum :: !Bool
  , nodes :: !(Maybe Quorum)
  , notfoundOk :: !(Maybe Bool)
  , pr :: !(Maybe Quorum)
  , r :: !(Maybe Quorum)
  , timeout :: !(Maybe Word32) -- TODO NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { basicQuorum = False
      , nodes = Nothing
      , notfoundOk = Nothing
      , pr = Nothing
      , r = Nothing
      , timeout = Nothing
      }

data PutOpts
  = PutOpts
  { dw :: !(Maybe Quorum)
  , nodes :: !(Maybe Quorum)
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
  -> m (Either (Error 'GetOp) (Object [Sibling ()]))
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
  -> m (Either (Error 'GetOp) (Maybe (Object [Sibling ByteString])))
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
  -> m (Either (Error 'GetOp) (Maybe (Object [Sibling ()])))
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
  -> IO (Either (Error 'GetOp) Proto.RpbGetResp)
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
            BucketTypeDoesNotExistError (request ^. Proto.type')
        | isInvalidNodesError err ->
            InvalidNodesError (request ^. Proto.nVal)
        | otherwise ->
            UnknownError (decodeUtf8 err)

makeGetRequest :: Key -> GetOpts -> Proto.RpbGetReq
makeGetRequest
    key
    GetOpts { basicQuorum, nodes, notfoundOk, pr, r, timeout } =

  Proto.defMessage
    & Key.setProto key
    & Proto.deletedvclock .~ True
    & Proto.maybe'basicQuorum .~ defFalse basicQuorum
    & Proto.maybe'notfoundOk .~ notfoundOk
    & Proto.maybe'nVal .~ (Quorum.toWord32 <$> nodes)
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
  -> IO (Either (Error 'PutOp) Key)
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
  -> m (Either (Error 'PutOp) (Object (NonEmpty (Sibling ByteString))))
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
  -> m (Either (Error 'PutOp) (Object (NonEmpty (Sibling ()))))
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
  -> IO (Either (Error 'PutOp) Proto.RpbPutResp)
doPut handle request =
  first parsePutError <$> Handle.put handle request

  where
    parsePutError :: Handle.Error -> Error 'PutOp
    parsePutError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isBucketTypeDoesNotExistError err ->
            BucketTypeDoesNotExistError (request ^. Proto.type')
        | isInvalidNodesError err ->
            InvalidNodesError (request ^. Proto.nVal)
        | otherwise ->
            UnknownError (decodeUtf8 err)

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
    & Proto.maybe'nVal .~ (Quorum.toWord32 <$> nodes)
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
  -> m (Either (Error 'DeleteOp) ())
delete
    handle
    Object { context, key }
    DeleteOpts { dw, nodes, pr, pw, r, timeout, w } = liftIO $

  first parseDeleteError <$> Handle.delete handle request

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
