module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , getBucket
  -- , setBucket
  , resetBucket
    -- ** Search
  , queryExact
  , queryRange
    -- ** Full traversals
  , listKeys
  , streamKeys
  ) where

import Libriak.Handle           (Handle)
import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.Error
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)

import qualified Libriak.Handle                    as Handle
import qualified Libriak.Proto                     as Proto
import qualified Riak.Internal.ExactQuery          as ExactQuery
import qualified Riak.Internal.RangeQuery          as RangeQuery
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, view, (.~), (^.))
import Data.Profunctor    (lmap)
import Data.Text.Encoding (decodeUtf8)

import qualified Control.Foldl as Foldl


-- | Get bucket properties.
--
-- TODO BucketProps
getBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either Handle.Error Proto.RpbBucketProps)
getBucket handle (Bucket bucketType bucket) = liftIO $
  Handle.getBucket handle request

  where
    request :: Proto.RpbGetBucketReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.type' .~ bucketType

-- | Set bucket properties.
--
-- TODO better set bucket properties type
-- TODO don't allow setting n
setBucket
  :: Handle -- ^
  -> Proto.RpbSetBucketReq -- ^
  -> IO (Either Handle.Error ())
setBucket handle request =
  Handle.setBucket handle request

-- | Reset bucket properties.
resetBucket ::
     MonadIO m
  => Handle
  -> Bucket
  -> m (Either Handle.Error ())
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
  -> IO (Either Handle.Error r)
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
  -> IO (Either Handle.Error r)
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
  -> IO (Either Handle.Error r)
doIndex handle =
  loop

  where
    loop ::
         Proto.RpbIndexReq
      -> FoldM IO Proto.RpbIndexResp r
      -> IO (Either Handle.Error r)
    loop request responseFold = do
      result :: Either Handle.Error (FoldM IO Proto.RpbIndexResp r, Maybe ByteString) <-
        doIndexPage
          handle
          request
          (Foldl.duplicateM responseFold)

      case result of
        Left err ->
          pure (Left err)

        Right (nextResponseFold, continuation) ->
          case continuation of
            Nothing ->
              Right <$> extractM nextResponseFold

            Just continuation -> do
              loop
                (request & Proto.continuation .~ continuation)
                nextResponseFold

doIndexPage ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either Handle.Error (r, Maybe ByteString))
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
streamKeys handle (Bucket bucketType bucket) keyFold = liftIO $
  first parseListKeysError <$>
    Handle.listKeys
      handle
      request
      (Foldl.handlesM (Proto.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    request :: Proto.RpbListKeysReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.type' .~ bucketType
        -- TODO stream keys timeout

    parseListKeysError :: Handle.Error -> Error 'ListKeysOp
    parseListKeysError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isBucketTypeDoesNotExistError_List err ->
            BucketTypeDoesNotExistError bucketType
        | otherwise ->
            UnknownError (decodeUtf8 err)


extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
