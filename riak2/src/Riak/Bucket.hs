module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , getBucket
  , setBucket
  , resetBucket
    -- ** Search
  , queryExact
  , queryRange
    -- ** Full traversals
  , listKeys
  , streamKeys
  ) where

import Riak.Handle              (Handle)
import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)

import qualified Riak.Handle                       as Handle
import qualified Riak.Internal.ExactQuery          as ExactQuery
import qualified Riak.Internal.RangeQuery          as RangeQuery
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue
import qualified Riak.Proto                        as Proto
import qualified Riak.Proto.Lens                   as L

import Control.Foldl   (FoldM(..))
import Control.Lens    (folded, to, view, (.~), (^.))
import Data.Profunctor (lmap)

import qualified Control.Foldl as Foldl


-- | Get bucket properties.
--
-- TODO BucketProps
getBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either Handle.Error Proto.BucketProperties)
getBucket handle (Bucket bucketType bucket) = liftIO $
  Handle.getBucket handle request

  where
    request :: Proto.GetBucketRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

-- | Set bucket properties.
--
-- TODO better set bucket properties type
-- TODO don't allow setting n
setBucket
  :: Handle -- ^
  -> Proto.SetBucketRequest -- ^
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
    request :: Proto.ResetBucketRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

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
    (Foldl.handlesM (L.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    Bucket bucketType bucket =
      ExactQuery.bucket query

    request :: Proto.SecondaryIndexRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.index .~ ExactQuery.name query
        & L.key .~ SecondaryIndexValue.encode value
        & L.maxResults .~ 50
        & L.stream .~ True
        & L.type' .~ Proto.SecondaryIndexRequest'exact

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
    (Foldl.handlesM (L.results . folded . to fromResult) keyFold)

  where
    Bucket bucketType bucket =
      RangeQuery.bucket query

    request :: Proto.SecondaryIndexRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.index .~ RangeQuery.name query
        & L.maxResults .~ 50 -- TODO configure page size
        & L.rangeMax .~ SecondaryIndexValue.encode (RangeQuery.max query)
        & L.rangeMax .~ SecondaryIndexValue.encode (RangeQuery.min query)
        & L.returnTerms .~ True
        & L.stream .~ True
        & L.type' .~ Proto.SecondaryIndexRequest'range

    fromResult :: Proto.Pair -> (a, Key)
    fromResult pair =
      ( case RangeQuery.min query of
          SecondaryIndexValue.Binary{}  -> pair ^. L.key
          SecondaryIndexValue.Integer{} -> bs2int (pair ^. L.key)
      , Key bucketType bucket (pair ^. L.value)
      )

doIndex ::
     forall r.
     Handle
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either Handle.Error r)
doIndex handle =
  loop

  where
    loop ::
         Proto.SecondaryIndexRequest
      -> FoldM IO Proto.SecondaryIndexResponse r
      -> IO (Either Handle.Error r)
    loop request responseFold = do
      result :: Either Handle.Error (FoldM IO Proto.SecondaryIndexResponse r, Maybe ByteString) <-
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

            Just continuation ->
              loop
                (request & L.continuation .~ continuation)
                nextResponseFold

doIndexPage ::
     Handle
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either Handle.Error (r, Maybe ByteString))
doIndexPage handle request fold =
  Handle.secondaryIndex
    handle
    request
    ((,)
      <$> fold
      <*> continuation)

  where
    continuation :: FoldM IO Proto.SecondaryIndexResponse (Maybe ByteString)
    continuation =
      Foldl.last
        & lmap (view L.maybe'continuation)
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
  -> m (Either Handle.Error [Key])
listKeys handle bucket =
  liftIO (streamKeys handle bucket (Foldl.generalize Foldl.list))

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'listKeys'
streamKeys
  :: Handle -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either Handle.Error r)
streamKeys handle (Bucket bucketType bucket) keyFold =
  Handle.listKeys
    handle
    request
    (Foldl.handlesM (L.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    request :: Proto.ListKeysRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        -- TODO stream keys timeout


extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
