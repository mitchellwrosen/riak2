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
  , listBucketKeys
  , streamBucketKeys
  ) where

import Riak.Client              (Client)
import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)

import qualified Riak.Interface                    as Interface
import qualified Riak.Internal.ExactQuery          as ExactQuery
import qualified Riak.Internal.RangeQuery          as RangeQuery
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue
import qualified Riak.Proto                        as Proto
import qualified Riak.Proto.Lens                   as L

import Control.Lens (folded, to)

import qualified Control.Foldl as Foldl


-- | Get bucket properties.
--
-- TODO BucketProps
getBucket ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> m (Either ByteString Proto.BucketProperties)
getBucket client (Bucket bucketType bucket) = liftIO $
  Interface.getBucket client request

  where
    request :: Proto.GetBucketRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

-- | Set bucket properties.
--
-- TODO better set bucket properties type
-- TODO don't allow setting n
setBucket
  :: Client -- ^
  -> Proto.SetBucketRequest -- ^
  -> IO (Either ByteString ())
setBucket client request =
  Interface.setBucket client request

-- | Reset bucket properties.
resetBucket ::
     MonadIO m
  => Client
  -> Bucket
  -> m (Either ByteString ())
resetBucket client (Bucket bucketType bucket) = liftIO $
  Interface.resetBucket client request

  where
    request :: Proto.ResetBucketRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

-- | Perform an exact query on a secondary index.
--
-- Fetches results in batches of 50.
queryExact
  :: Client -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either ByteString r)
queryExact client query@(ExactQuery { value }) keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    Bucket bucketType bucket =
      ExactQuery.bucket query

    request :: Proto.SecondaryIndexRequest
    request =
      defMessage
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
     Client -- ^
  -> RangeQuery a -- ^
  -> FoldM IO (a, Key) r -- ^
  -> IO (Either ByteString r)
queryRange client query keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.results . folded . to fromResult) keyFold)

  where
    Bucket bucketType bucket =
      RangeQuery.bucket query

    request :: Proto.SecondaryIndexRequest
    request =
      defMessage
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
     Client
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either ByteString r)
doIndex client =
  loop

  where
    loop ::
         Proto.SecondaryIndexRequest
      -> FoldM IO Proto.SecondaryIndexResponse r
      -> IO (Either ByteString r)
    loop request responseFold = do
      result :: Either ByteString (FoldM IO Proto.SecondaryIndexResponse r, Maybe ByteString) <-
        doIndexPage
          client
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
     Client
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either ByteString (r, Maybe ByteString))
doIndexPage client request fold =
  Interface.secondaryIndex
    client
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
-- This is 'streamBucketKeys' with a simpler type, but pulls all keys into
-- memory before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'streamBucketKeys'
listBucketKeys ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> m (Either ByteString [Key])
listBucketKeys client bucket =
  liftIO (streamBucketKeys client bucket (Foldl.generalize Foldl.list))

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'listBucketKeys'
streamBucketKeys
  :: Client -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either ByteString r)
streamBucketKeys client (Bucket bucketType bucket) keyFold =
  Interface.listKeys
    client
    request
    (Foldl.handlesM (L.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    request :: Proto.ListKeysRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        -- TODO stream keys timeout


extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
