module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
    -- ** Properties
  , get
  , set
  , reset
    -- ** Search
  , exactQuery
  , rangeQuery
    -- ** Full traversals
  , keys
  , streamKeys
  ) where

import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.Client     (Client)
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Internal.Client              as Client
import qualified Riak.Internal.ExactQuery          as ExactQuery
import qualified Riak.Internal.RangeQuery          as RangeQuery
import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue
import qualified Riak.Proto                        as Proto
import qualified Riak.Proto.Lens                   as L

import Control.Foldl (FoldM(..))
import Control.Lens  (folded, to)

import qualified Control.Foldl as Foldl


-- | Get bucket properties.
--
-- TODO BucketProps
get ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> m (Either Text Proto.BucketProperties)
get client (Bucket bucketType bucket) = liftIO $
  (fmap.fmap)
    fromResponse
    (Client.exchange
      client
      (RequestGetBucketProperties request)
      (\case
        ResponseGetBucketProperties response -> Just response
        _ -> Nothing))

  where
    request :: Proto.GetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

    fromResponse :: Proto.GetBucketPropertiesResponse -> Proto.BucketProperties
    fromResponse =
      view L.props

-- | Set bucket properties.
--
-- TODO better set bucket properties type
-- TODO don't allow setting n
set
  :: Client -- ^
  -> Proto.SetBucketPropertiesRequest -- ^
  -> IO (Either Text Proto.SetBucketPropertiesResponse)
set client request =
  Client.exchange
    client
    (RequestSetBucketProperties request)
    (\case
      ResponseSetBucketProperties response -> Just response
      _ -> Nothing)

-- | Reset bucket properties.
reset ::
     MonadIO m
  => Client
  -> Bucket
  -> m (Either Text ())
reset client (Bucket bucketType bucket) = liftIO $
  Client.exchange
    client
    (RequestResetBucketProperties request)
    (\case
      ResponseResetBucketProperties _ -> Just ()
      _ -> Nothing)

  where
    request :: Proto.ResetBucketPropertiesRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType

-- | List all of the keys in a bucket.
--
-- This is 'streamKeys' with a simpler type, but pulls all keys into memory
-- before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'streamKeys'
keys ::
     MonadIO m
  => Client -- ^
  -> Bucket -- ^
  -> m (Either Text [Key])
keys client bucket =
  liftIO (streamKeys client bucket (Foldl.generalize Foldl.list))

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.ExactQuery.inBucket' query.
--
-- /See also/: 'keys'
streamKeys
  :: Client -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either Text r)
streamKeys client (Bucket bucketType bucket) keyFold =
  Client.stream
    client
    (RequestListKeys request)
    (\case
      ResponseListKeys response -> Just response
      _ -> Nothing)
    (view L.done)
    (Foldl.handlesM (L.keys . folded . to (Key bucketType bucket)) keyFold)

  where
    request :: Proto.ListKeysRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        -- TODO stream keys timeout

-- | Perform an exact query on a secondary index.
--
-- Fetches results in batches of 50.
exactQuery
  :: Client -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Either Text r)
exactQuery client query@(ExactQuery { value }) keyFold =
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
rangeQuery
  :: forall a r.
     Client -- ^
  -> RangeQuery a -- ^
  -> FoldM IO (a, Key) r -- ^
  -> IO (Either Text r)
rangeQuery client query keyFold =
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
  -> IO (Either Text r)
doIndex client =
  loop

  where
    loop ::
         Proto.SecondaryIndexRequest
      -> FoldM IO Proto.SecondaryIndexResponse r
      -> IO (Either Text r)
    loop request responseFold = do
      result :: Either Text (FoldM IO Proto.SecondaryIndexResponse r, Maybe ByteString) <-
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
  -> IO (Either Text (r, Maybe ByteString))
doIndexPage client request fold =
  Client.stream
    client
    (RequestSecondaryIndex request)
    (\case
      ResponseSecondaryIndex response -> Just response
      _ -> Nothing)
    (view L.done)
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

extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
