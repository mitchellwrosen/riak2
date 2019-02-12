module Riak.Object
  ( -- * Object
    Object(..)
  , newObject
    -- ** Get object
  , get
  , getHead
  , getIfModified
  , getHeadIfModified
  , GetOpts(..)
    -- ** Put object
  , put
  , putGet
  , putGetHead
  , PutOpts(..)
    -- ** Delete object
  , delete
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
import qualified Libriak.Proto.Lens           as L
import qualified Riak.Internal.Key            as Key
import qualified Riak.Internal.Proto.Pair     as Proto.Pair
import qualified Riak.Internal.Quorum         as Quorum
import qualified Riak.Internal.SecondaryIndex as SecondaryIndex

import Control.Lens          ((.~), (^.))
import Data.Default.Class    (Default(..))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)

import qualified ByteString


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
        | isInvalidNodesError err ->
            InvalidNodesError (request ^. L.nodes)
        | otherwise ->
            UnknownError (decodeUtf8 err)

makeGetRequest :: Key -> GetOpts -> Proto.GetRequest
makeGetRequest
    key
    GetOpts { basicQuorum, nodes, notfoundOk, pr, r, timeout } =

  Proto.defMessage
    & Key.setProto key
    & L.deletedContext .~ True
    & L.maybe'basicQuorum .~ defFalse basicQuorum
    & L.maybe'nodes .~ (Quorum.toWord32 <$> nodes)
    & L.maybe'notfoundOk .~ notfoundOk
    & L.maybe'pr .~ (Quorum.toWord32 <$> pr)
    & L.maybe'r .~ (Quorum.toWord32 <$> r)
    & L.maybe'timeout .~ timeout


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
        | isInvalidNodesError err ->
            InvalidNodesError (request ^. L.nodes)
        | otherwise ->
            UnknownError (decodeUtf8 err)

makePutRequest ::
     Object (Content ByteString)
  -> PutOpts
  -> Proto.PutRequest
makePutRequest
     Object { content = Content { charset, encoding, indexes, metadata, type', value }, context, key }
     PutOpts { dw, nodes, pw, timeout, w } =
  Proto.defMessage
    & Key.setMaybeProto key
    & L.content .~
        (Proto.defMessage
          & L.indexes .~ map SecondaryIndex.toPair indexes
          & L.maybe'charset .~ charset
          & L.maybe'contentEncoding .~ encoding
          & L.maybe'contentType .~ type'
          & L.usermeta .~ map Proto.Pair.fromTuple metadata
          & L.value .~ value
        )
    & L.maybe'context .~
        (if ByteString.null (unContext context)
          then Nothing
          else Just (unContext context))
    & L.maybe'dw .~ (Quorum.toWord32 <$> dw)
    & L.maybe'nodes .~ (Quorum.toWord32 <$> nodes)
    & L.maybe'pw .~ (Quorum.toWord32 <$> pw)
    & L.maybe'w .~ (Quorum.toWord32 <$> w)
    & L.maybe'timeout .~ timeout

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
    request :: Proto.DeleteRequest
    request =
      Proto.defMessage
        & Key.setProto key
        & L.maybe'dw .~ (Quorum.toWord32 <$> dw)
        & L.maybe'nodes .~ (Quorum.toWord32 <$> nodes)
        & L.maybe'pr .~ (Quorum.toWord32 <$> pr)
        & L.maybe'pw .~ (Quorum.toWord32 <$> pw)
        & L.maybe'r .~ (Quorum.toWord32 <$> r)
        & L.maybe'timeout .~ timeout
        & L.maybe'w .~ (Quorum.toWord32 <$> w)
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
