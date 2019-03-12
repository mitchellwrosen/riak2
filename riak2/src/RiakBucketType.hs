module RiakBucketType
  ( BucketType
  , defaultBucketType
  , getBucketType
  , getCounterBucketType
  , getHyperLogLogBucketType
  , getMapBucketType
  , getSetBucketType
  , setBucketTypeIndex
  , unsetBucketTypeIndex
  , listBuckets
  , streamBuckets

  , coerceGetBucketError
  ) where

import RiakBucketInternal         (Bucket(..))
import RiakBucketTypeInternal     (BucketType, defaultBucketType)
import RiakCounterBucketProps     (CounterBucketProps)
import RiakError
import RiakHandle                 (Handle, HandleError)
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps)
import RiakIndexName              (IndexName(..))
import RiakMapBucketProps         (MapBucketProps)
import RiakSetBucketProps         (SetBucketProps)
import RiakSomeBucketProps        (SomeBucketProps)
import RiakSomeBucketProps        (SomeBucketProps(..))
import RiakUtils                  (retrying)

import qualified RiakHandle          as Handle
import qualified RiakSomeBucketProps as SomeBucketProps

import Control.Foldl      (FoldM(..))
import Control.Lens       (folded, to, (.~), (^.))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unsafe.Coerce      (unsafeCoerce)

import qualified Control.Foldl   as Foldl
import qualified Data.Riak.Proto as Proto


-- | Get a bucket type's properties.
--
-- If you know the bucket type's type ahead of time, prefer
-- 'getCounterBucketType', 'getHyperLogLogBucketType', 'getMapBucketType', or
-- 'getSetBucketType'.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
getBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetBucketTypeError SomeBucketProps)
getBucketType handle bucketType = liftIO $
  fromResult <$> Handle.getBucketType handle bucketType

  where
    fromResult ::
         Either HandleError (Either ByteString Proto.RpbGetBucketResp)
      -> Either GetBucketTypeError SomeBucketProps
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseGetBucketTypeError bucketType err)

      Right (Right response) ->
        Right (SomeBucketProps.fromProto (response ^. Proto.props))

-- | Get a counter bucket type's properties.
--
-- Prefer this to 'getBucketType' if you are certain the given bucket type
-- contains counters.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
-- | 'InvalidBucketTypeError'      | The bucket does not contain counters      |
-- |                               | (@datatype = counter@).                   |
-- +-------------------------------+-------------------------------------------+
getCounterBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetCounterBucketError CounterBucketProps)
getCounterBucketType handle bucketType =
  fromResult <$> getBucketType handle bucketType

  where
    fromResult ::
         Either GetBucketTypeError SomeBucketProps
      -> Either GetCounterBucketTypeError CounterBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeCounterBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a HyperLogLog bucket type's properties.
--
-- Prefer this to 'getBucketType' if you are certain the given bucket type
-- contains HyperLogLogs.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
-- | 'InvalidBucketTypeError'      | The bucket does not contain HyperLogLogs  |
-- |                               | (@datatype = hll@).                       |
-- +-------------------------------+-------------------------------------------+
getHyperLogLogBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetHyperLogLogBucketError HyperLogLogBucketProps)
getHyperLogLogBucketType handle bucketType =
  fromResult <$> getBucketType handle bucketType

  where
    fromResult ::
         Either GetBucketTypeError SomeBucketProps
      -> Either GetHyperLogLogBucketTypeError HyperLogLogBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeHyperLogLogBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a map bucket type's properties.
--
-- Prefer this to 'getBucketType' if you are certain the given bucket type
-- contains maps.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
-- | 'InvalidBucketTypeError'      | The bucket does not contain maps          |
-- |                               | (@datatype = map@).                       |
-- +-------------------------------+-------------------------------------------+
getMapBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetMapBucketError MapBucketProps)
getMapBucketType handle bucketType =
  fromResult <$> getBucketType handle bucketType

  where
    fromResult ::
         Either GetBucketTypeError SomeBucketProps
      -> Either GetMapBucketTypeError MapBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeMapBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a set bucket type's properties.
--
-- Prefer this to 'getBucketType' if you are certain the given bucket type
-- contains sets.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
-- | 'InvalidBucketTypeError'      | The bucket does not contain sets          |
-- |                               | (@datatype = set@).                       |
-- +-------------------------------+-------------------------------------------+
getSetBucketType ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either GetSetBucketError SetBucketProps)
getSetBucketType handle bucketType =
  fromResult <$> getBucketType handle bucketType

  where
    fromResult ::
         Either GetBucketTypeError SomeBucketProps
      -> Either GetSetBucketTypeError SetBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeSetBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

parseGetBucketTypeError ::
     BucketType
  -> ByteString
  -> GetBucketTypeError
parseGetBucketTypeError bucketType err
  | isBucketTypeDoesNotExistError3 err =
      BucketTypeDoesNotExistError bucketType
  | otherwise =
      UnknownError (decodeUtf8 err)

coerceGetBucketError ::
     MayReturnBucketTypeDoesNotExist op ~ 'True
  => GetBucketError
  -> Error op
coerceGetBucketError =
  {-
  \case
    BucketTypeDoesNotExistError x -> BucketTypeDoesNotExistError x
    HandleError                 x -> HandleError                 x
    UnknownError                x -> UnknownError                x
  -}
  unsafeCoerce

-- | Set the index of a bucket type.
--
-- If given the default bucket type, returns 'BucketTypeInvalid', because its
-- properties cannot be changed.
--
-- If the search index's @nodes@ value does not match the bucket's, returns
-- '
-- properties cannot be changed.
--
-- /Note/: If search is not enabled, Riak does not complain if you associate a
-- bucket type with an index that does exist.
--
-- /See also/: 'Riak.Index.putIndex'
setBucketTypeIndex ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> IndexName -- ^ Index name
  -> m (Either SetBucketTypeIndexError ())
setBucketTypeIndex handle bucketType index
  -- Careful changing this code... does it still make sense for
  -- 'unsetBucketTypeIndex' to share an error type?
  | bucketType == defaultBucketType =
      pure (Left (InvalidBucketTypeError bucketType))
  | otherwise =
      setBucketTypeIndex_ handle bucketType index

setBucketTypeIndex_ ::
     MonadIO m
  => Handle
  -> BucketType
  -> IndexName
  -> m (Either SetBucketTypeIndexError ())
setBucketTypeIndex_ handle bucketType index = liftIO $
  Handle.setBucketType handle request >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseSetBucketTypeIndexError bucketType index err))

    Right (Right _) ->
      pure (Right ())

  where
    request :: Proto.RpbSetBucketTypeReq
    request =
      Proto.defMessage
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 (unIndexName index))
        & Proto.type' .~ bucketType

-- | Unset the index of a bucket type.
unsetBucketTypeIndex ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either SetBucketTypeIndexError ())
unsetBucketTypeIndex handle bucketType =
  setBucketTypeIndex handle bucketType (IndexName "_dont_index_")

parseSetBucketTypeIndexError ::
     ByteString
  -> IndexName
  -> ByteString
  -> SetBucketTypeIndexError
parseSetBucketTypeIndexError bucketType index err
  | isBucketTypeDoesNotExistError2 err =
      BucketTypeDoesNotExistError bucketType
  | isIndexDoesNotExistError0 err =
      IndexDoesNotExistError index
  | isInvalidNodesError1 err =
      InvalidNodesError
  | otherwise =
      UnknownError (decodeUtf8 err)

-- | List all of the buckets in a bucket type.
--
-- This is 'streamBuckets' with a simpler type, but pulls all keys into memory
-- before returning them.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'streamBuckets'
listBuckets ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> m (Either ListBucketsError [Bucket])
listBuckets handle bucketType =
  streamBuckets handle bucketType (Foldl.generalize Foldl.list)

-- | Stream all of the buckets in a bucket type.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /See also/: 'listBuckets'
streamBuckets ::
     MonadIO m
  => Handle -- ^
  -> BucketType -- ^
  -> FoldM IO Bucket r -- ^
  -> m (Either ListBucketsError r)
streamBuckets handle bucketType bucketFold =
  liftIO (retrying 1000000 (streamBuckets_ handle bucketType bucketFold))

streamBuckets_ ::
     Handle
  -> BucketType
  -> FoldM IO Bucket r
  -> IO (Maybe (Either ListBucketsError r))
streamBuckets_ handle bucketType bucketFold =
  Handle.listBuckets handle request (makeResponseFold bucketType bucketFold) >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseListBucketsError bucketType err)

    Right (Right response) ->
      pure (Just (Right response))

  where
    request :: Proto.RpbListBucketsReq
    request =
      Proto.defMessage
        & Proto.stream .~ True
        & Proto.type' .~ bucketType
        -- TODO stream buckets timeout

parseListBucketsError :: ByteString -> ByteString -> Maybe ListBucketsError
parseListBucketsError bucketType err
  | isBucketTypeDoesNotExistError4 err =
      Just (BucketTypeDoesNotExistError bucketType)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

makeResponseFold ::
     forall m r. Monad m
  => ByteString
  -> FoldM m Bucket r
  -> FoldM m Proto.RpbListBucketsResp r
makeResponseFold bucketType =
  Foldl.handlesM handler

  where
    handler :: Foldl.HandlerM m Proto.RpbListBucketsResp Bucket
    handler =
      Proto.buckets . folded . to (Bucket bucketType)
