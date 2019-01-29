module Riak.Bucket
  ( -- * Bucket
    Bucket(..)
  , get
  , set
  , reset
  , keys
  , streamKeys
    -- ** Search
  , exactQuery
  , rangeQuery
  ) where

import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.Client     (Client, Result(..))
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Internal.Client     as Client
import qualified Riak.Internal.ExactQuery as ExactQuery
import qualified Riak.Internal.IndexValue as IndexValue
import qualified Riak.Internal.RangeQuery as RangeQuery
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

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
  -> m (Result Proto.BucketProperties)
get client (Bucket type' bucket) = liftIO $
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
        & L.type' .~ type'

    fromResponse :: Proto.GetBucketPropertiesResponse -> Proto.BucketProperties
    fromResponse =
      view L.props

-- | Set bucket properties.
-- TODO better set bucket properties type
set
  :: Client -- ^
  -> Proto.SetBucketPropertiesRequest -- ^
  -> IO (Result Proto.SetBucketPropertiesResponse)
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
  -> m (Result ())
reset client (Bucket type' bucket) = liftIO $
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
        & L.type' .~ type'

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
  -> m (Result [Key])
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
  -> IO (Result r)
streamKeys client (Bucket type' bucket) keyFold =
  Client.stream
    client
    (RequestListKeys request)
    (\case
      ResponseListKeys response -> Just response
      _ -> Nothing)
    (view L.done)
    (Foldl.handlesM (L.keys . folded . to (Key type' bucket)) keyFold)

  where
    request :: Proto.ListKeysRequest
    request =
      defMessage
        & L.type' .~ type'
        & L.bucket .~ bucket
        -- TODO stream keys timeout

-- | Perform an exact query on a secondary index.
--
-- Fetches results in batches of 50.
exactQuery
  :: Client -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
exactQuery client query@(ExactQuery { value }) keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.keys . folded . to (Key type' bucket)) keyFold)

  where
    Bucket type' bucket =
      ExactQuery.bucket query

    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ ExactQuery.name query
        & L.key .~ IndexValue.encode value
        & L.maxResults .~ 50
        & L.qtype .~ Proto.IndexRequest'exact
        & L.stream .~ True
        & L.type' .~ type'

-- | Perform a range query on a secondary index.
--
-- Fetches results in batches of 50.
rangeQuery
  :: forall a r.
     Client -- ^
  -> RangeQuery a -- ^
  -> FoldM IO (a, Key) r -- ^
  -> IO (Result r)
rangeQuery client query keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.results . folded . to fromResult) keyFold)

  where
    Bucket type' bucket =
      RangeQuery.bucket query

    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ RangeQuery.name query
        & L.maxResults .~ 50 -- TODO configure page size
        & L.qtype .~ Proto.IndexRequest'range
        & L.rangeMax .~ IndexValue.encode (RangeQuery.max query)
        & L.rangeMax .~ IndexValue.encode (RangeQuery.min query)
        & L.returnTerms .~ True
        & L.stream .~ True
        & L.type' .~ type'

    fromResult :: Proto.Pair -> (a, Key)
    fromResult pair =
      ( case RangeQuery.min query of
          IndexValue.Binary{}  -> pair ^. L.key
          IndexValue.Integer{} -> bs2int (pair ^. L.key)
      , Key type' bucket (pair ^. L.value)
      )

doIndex ::
     forall r.
     Client
  -> Proto.IndexRequest
  -> FoldM IO Proto.IndexResponse r
  -> IO (Result r)
doIndex client =
  loop

  where
    loop :: Proto.IndexRequest -> FoldM IO Proto.IndexResponse r -> IO (Result r)
    loop request responseFold = do
      result :: Result (FoldM IO Proto.IndexResponse r, Maybe ByteString) <-
        doIndexPage
          client
          request
          (Foldl.duplicateM responseFold)

      case result of
        ConnectionClosed ->
          pure ConnectionClosed

        Failure err ->
          pure (Failure err)

        Success (nextResponseFold, continuation) ->
          case continuation of
            Nothing ->
              Success <$> extractM nextResponseFold

            Just continuation ->
              loop
                (request & L.continuation .~ continuation)
                nextResponseFold

doIndexPage ::
     Client
  -> Proto.IndexRequest
  -> FoldM IO Proto.IndexResponse r
  -> IO (Result (r, Maybe ByteString))
doIndexPage client request fold =
  Client.stream
    client
    (RequestIndex request)
    (\case
      ResponseIndex response -> Just response
      _ -> Nothing)
    (view L.done)
    ((,)
      <$> fold
      <*> continuation)

  where
    continuation :: FoldM IO Proto.IndexResponse (Maybe ByteString)
    continuation =
      Foldl.last
        & lmap (view L.maybe'continuation)
        & Foldl.generalize
        & fmap join

extractM :: Monad m => FoldM m a r -> m r
extractM (FoldM _ x f) =
  x >>= f
