module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , getBucket
  , setBucketIndex
  , resetBucket
    -- ** Search
  , queryExact
  , queryRange
    -- ** Full traversals
  , listKeys
  , streamKeys
  ) where

import Libriak.Handle                 (Handle)
import Riak.Internal.Bucket           (Bucket(..))
import Riak.Internal.BucketProperties (BucketProperties)
import Riak.Internal.Error
import Riak.Internal.ExactQuery       (ExactQuery(..))
import Riak.Internal.IndexName        (IndexName(..))
import Riak.Internal.Key              (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery       (RangeQuery)
import Riak.Internal.Utils            (bs2int)

import qualified Libriak.Handle                    as Handle
import qualified Libriak.Proto                     as Proto
import qualified Riak.Internal.BucketProperties    as BucketProperties
import qualified Riak.Internal.ExactQuery          as ExactQuery
import qualified Riak.Internal.RangeQuery          as RangeQuery
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, view, (.~), (^.))
import Data.Profunctor    (lmap)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Control.Foldl as Foldl


-- | Get bucket properties.
getBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either (Error 'GetBucketOp) (Maybe BucketProperties))
getBucket handle bucket = liftIO $
  fromHandleResult
    parseGetBucketError
    (Just . BucketProperties.fromProto)
    (Handle.getBucket handle request)

  where
    request :: Proto.RpbGetBucketReq
    request =
      Proto.defMessage
        & setProto bucket

parseGetBucketError ::
     ByteString
  -> Either (Error 'GetBucketOp) (Maybe BucketProperties)
parseGetBucketError err
  | isBucketTypeDoesNotExistError4 err =
      Right Nothing
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

-- | Set the index of a bucket.
setBucketIndex ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> IndexName -- ^ Index name
  -> m (Either Handle.HandleError (Either ByteString ()))
setBucketIndex handle bucket (IndexName index) =
  liftIO (Handle.setBucket handle request)

  where
    request :: Proto.RpbSetBucketReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 index)

-- | Set bucket properties.
setBucket
  :: Handle -- ^
  -> Proto.RpbSetBucketReq -- ^
  -> IO (Either Handle.HandleError (Either ByteString ()))
setBucket handle request =
  Handle.setBucket handle request

-- | Reset bucket properties.
resetBucket ::
     MonadIO m
  => Handle
  -> Bucket
  -> m (Either Handle.HandleError (Either ByteString ()))
resetBucket handle (Bucket bucketType bucket) = liftIO $
  Handle.resetBucket handle request

  where
    request :: Proto.RpbResetBucketReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.type' .~ bucketType

-- | Perform an exact query on a secondary index.
--
-- Fetches results in batches of 50.
queryExact
  :: Handle -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either Handle.HandleError (Either ByteString r))
queryExact handle query@(ExactQuery { value }) keyFold =
  doIndex
    handle
    request
    (Foldl.handlesM (Proto.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    Bucket bucketType bucket =
      ExactQuery.bucket query

    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.index .~ ExactQuery.name query
        & Proto.key .~ SecondaryIndexValue.encode value
        & Proto.maxResults .~ 50
        & Proto.qtype .~ Proto.RpbIndexReq'eq
        & Proto.stream .~ True
        & Proto.type' .~ bucketType

-- | Perform a range query on a secondary index.
--
-- Fetches results in batches of 50.
queryRange
  :: forall a r.
     Handle -- ^
  -> RangeQuery a -- ^
  -> FoldM IO (a, Key) r -- ^
  -> IO (Either Handle.HandleError (Either ByteString r))
queryRange handle query keyFold =
  doIndex
    handle
    request
    (Foldl.handlesM (Proto.results . folded . to fromResult) keyFold)

  where
    Bucket bucketType bucket =
      RangeQuery.bucket query

    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.index .~ RangeQuery.name query
        & Proto.maxResults .~ 50 -- TODO configure page size
        & Proto.rangeMax .~ SecondaryIndexValue.encode (RangeQuery.max query)
        & Proto.rangeMax .~ SecondaryIndexValue.encode (RangeQuery.min query)
        & Proto.returnTerms .~ True
        & Proto.qtype .~ Proto.RpbIndexReq'range
        & Proto.stream .~ True
        & Proto.type' .~ bucketType

    fromResult :: Proto.RpbPair -> (a, Key)
    fromResult pair =
      ( case RangeQuery.min query of
          SecondaryIndexValue.Binary{}  -> pair ^. Proto.key
          SecondaryIndexValue.Integer{} -> bs2int (pair ^. Proto.key)
      , Key bucketType bucket (pair ^. Proto.value)
      )

doIndex ::
     forall r.
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either Handle.HandleError (Either ByteString r))
doIndex handle =
  loop

  where
    loop ::
         Proto.RpbIndexReq
      -> FoldM IO Proto.RpbIndexResp r
      -> IO (Either Handle.HandleError (Either ByteString r))
    loop request responseFold = do
      result :: Either Handle.HandleError (Either ByteString (FoldM IO Proto.RpbIndexResp r, Maybe ByteString)) <-
        doIndexPage
          handle
          request
          (Foldl.duplicateM responseFold)

      case result of
        Left err ->
          pure (Left err)

        Right (Left err) ->
          pure (Right (Left err))

        Right (Right (nextResponseFold, continuation)) ->
          case continuation of
            Nothing ->
              Right . Right <$> extractM nextResponseFold

            Just continuation -> do
              loop
                (request & Proto.continuation .~ continuation)
                nextResponseFold

doIndexPage ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either Handle.HandleError (Either ByteString (r, Maybe ByteString)))
doIndexPage handle request fold =
  Handle.secondaryIndex
    handle
    request
    ((,)
      <$> fold
      <*> continuation)

  where
    continuation :: FoldM IO Proto.RpbIndexResp (Maybe ByteString)
    continuation =
      Foldl.last
        & lmap (view Proto.maybe'continuation)
        & Foldl.generalize
        & fmap join

-- | List all of the keys in a bucket.
--
-- This is 'streamKeys' with a simpler type, but pulls all keys into
-- memory before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'streamKeys'
listKeys ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either (Error 'ListKeysOp) [Key])
listKeys handle bucket =
  streamKeys handle bucket (Foldl.generalize Foldl.list)

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'listKeys'
streamKeys ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> m (Either (Error 'ListKeysOp) r)
streamKeys handle b@(Bucket bucketType bucket) keyFold = liftIO $
  fromHandleResult
    (Left . parseListKeysError bucketType)
    id
    doRequest

  where
    doRequest =
      Handle.listKeys
        handle
        request
        (Foldl.handlesM
          (Proto.keys . folded . to (Key bucketType bucket))
          keyFold)

    request :: Proto.RpbListKeysReq
    request =
      Proto.defMessage
        & setProto b
        -- TODO stream keys timeout

parseListKeysError :: ByteString -> ByteString -> Error 'ListKeysOp
parseListKeysError bucketType err
  | isBucketTypeDoesNotExistError4 err =
      BucketTypeDoesNotExistError bucketType
  | otherwise =
      UnknownError (decodeUtf8 err)

setProto ::
     ( Proto.HasLens' a "bucket" ByteString
     , Proto.HasLens' a "maybe'type'" (Maybe ByteString)
     )
  => Bucket
  -> a
  -> a
setProto (Bucket bucketType bucket) proto =
  proto
    & Proto.bucket .~ bucket
    & Proto.maybe'type' .~ (do
        guard (bucketType /= "default")
        pure bucketType)

extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
