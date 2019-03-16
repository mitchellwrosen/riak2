module RiakBucket
  ( Bucket(..)
  , bucketBucketType
  , bucketBucketSegment
  , getBucket
  , getCounterBucket
  , getHyperLogLogBucket
  , getMapBucket
  , getSetBucket
  , setBucketIndex
  , unsetBucketIndex
  , resetBucket
  , queryIntIndex
  , queryIntIndexTerms
  , queryBinaryIndex
  , queryBinaryIndexTerms
  , listKeys
  , streamKeys

  , fromProto
  ) where

import RiakBinaryIndexQuery       (BinaryIndexQuery(..), builtinBucketIndex,
                                   builtinKeyIndex)
import RiakBucketInternal         (Bucket(..))
import RiakBucketType             (BucketType, coerceGetBucketError)
import RiakCounterBucketProps     (CounterBucketProps)
import RiakError
import RiakHandle                 (Handle)
import RiakHandleError            (HandleError)
import RiakHyperLogLogBucketProps (HyperLogLogBucketProps)
import RiakIndexName              (IndexName(..))
import RiakIntIndexQuery          (IntIndexQuery(..))
import RiakKey                    (Key(..))
import RiakMapBucketProps         (MapBucketProps)
import RiakSetBucketProps         (SetBucketProps)
import RiakSomeBucketProps        (SomeBucketProps(..))
import RiakUtils                  (bs2int, int2bs)

import qualified RiakBinaryIndexQuery as BinaryIndexQuery
import qualified RiakBucketType       as BucketType
import qualified RiakHandle           as Handle
import qualified RiakSomeBucketProps  as SomeBucketProps

import Control.Foldl                      (FoldM(..))
import Control.Lens                       (Lens', folded, to, (.~), (^.))
import Data.Profunctor                    (lmap)
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Data.Text.Encoding                 (decodeUtf8, encodeUtf8)

import qualified Control.Foldl   as Foldl
import qualified Data.Riak.Proto as Proto


-- | A lens onto the bucket type of a bucket.
--
-- @
-- Bucket bucketType bucket
--        `————————´
-- @
bucketBucketType :: Lens' Bucket BucketType
bucketBucketType f (Bucket bucketType bucket) =
  (\bucketType -> Bucket bucketType bucket) <$>
    f bucketType

-- | A lens onto the bucket segment of a bucket.
--
-- @
-- Bucket bucketType bucket
--                   `————´
-- @
bucketBucketSegment :: Lens' Bucket ByteString
bucketBucketSegment f (Bucket bucketType bucket) =
  (\bucket -> Bucket bucketType bucket) <$>
    f bucket

-- | Get a bucket's properties.
--
-- If you know the bucket's type ahead of time, prefer 'getCounterBucket',
-- 'getHyperLogLogBucket', 'getMapBucket', or 'getSetBucket'.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
getBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetBucketError SomeBucketProps)
getBucket handle bucket@(Bucket bucketType _) = liftIO $
  fromResult <$> Handle.getBucket handle request

  where
    request :: Proto.RpbGetBucketReq
    request =
      Proto.defMessage
        & setProto bucket

    fromResult ::
         Either [HandleError] (Either ByteString Proto.RpbGetBucketResp)
      -> Either GetBucketError SomeBucketProps
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseError err)

      Right (Right response) ->
        Right (SomeBucketProps.fromProto (response ^. Proto.props))

    parseError ::
         MayReturnBucketTypeDoesNotExist op ~ 'True
      => ByteString
      -> Error op
    parseError err
      | isBucketTypeDoesNotExistError4 err =
          BucketTypeDoesNotExistError bucketType
      | otherwise =
          UnknownError (decodeUtf8 err)



-- | Get a counter bucket's properties.
--
-- Prefer this to 'getBucket' if you are certain the given bucket contains
-- counters.
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
getCounterBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetCounterBucketError CounterBucketProps)
getCounterBucket handle bucket@(Bucket bucketType _) =
  fromResult <$> getBucket handle bucket

  where
    fromResult ::
         Either GetBucketError SomeBucketProps
      -> Either GetCounterBucketError CounterBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeCounterBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a HyperLogLog bucket's properties.
--
-- Prefer this to 'getBucket' if you are certain the given bucket contains
-- HyperLogLogs.
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
getHyperLogLogBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetHyperLogLogBucketError HyperLogLogBucketProps)
getHyperLogLogBucket handle bucket@(Bucket bucketType _) =
  fromResult <$> getBucket handle bucket

  where
    fromResult ::
         Either GetBucketError SomeBucketProps
      -> Either GetHyperLogLogBucketError HyperLogLogBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeHyperLogLogBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a map bucket's properties.
--
-- Prefer this to 'getBucket' if you are certain the given bucket contains
-- maps.
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
getMapBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetMapBucketError MapBucketProps)
getMapBucket handle bucket@(Bucket bucketType _) =
  fromResult <$> getBucket handle bucket

  where
    fromResult ::
         Either GetBucketError SomeBucketProps
      -> Either GetMapBucketError MapBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeMapBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Get a set bucket's properties.
--
-- Prefer this to 'getBucket' if you are certain the given bucket contains
-- sets.
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
getSetBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either GetSetBucketError SetBucketProps)
getSetBucket handle bucket@(Bucket bucketType _) =
  fromResult <$> getBucket handle bucket

  where
    fromResult ::
         Either GetBucketError SomeBucketProps
      -> Either GetSetBucketError SetBucketProps
    fromResult = \case
      Left err ->
        Left (coerceGetBucketError err)

      Right (SomeSetBucketProps props) ->
        Right props

      Right _ ->
        Left (InvalidBucketTypeError bucketType)

-- | Set the index of a bucket.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
-- | 'IndexDoesNotExistError'      | The index does not exist. You must first  |
-- |                               | create it with 'Riak.Index.putIndex'.     |
-- +-------------------------------+-------------------------------------------+
-- | 'InvalidNodesError'           | The index's @nodes@ value does not equal  |
-- |                               | the bucket's @nodes@ value.               |
-- +-------------------------------+-------------------------------------------+
setBucketIndex ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> IndexName -- ^ Index name
  -> m (Either SetBucketIndexError ())
setBucketIndex handle bucket@(Bucket bucketType _) index = liftIO $
  setBucketIndex_
    handle
    bucket
    index
    parseError

  where
    parseError ::
         ByteString
      -> SetBucketIndexError
    parseError err
      | isBucketTypeDoesNotExistError4 err =
          BucketTypeDoesNotExistError bucketType
      | isIndexDoesNotExistError0 err =
          IndexDoesNotExistError index
      | isInvalidNodesError1 err =
          InvalidNodesError
      | otherwise =
          UnknownError (decodeUtf8 err)

-- | Unset the index of a bucket.
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
unsetBucketIndex ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either UnsetBucketIndexError ())
unsetBucketIndex handle bucket@(Bucket bucketType _) = liftIO $
  setBucketIndex_
    handle
    bucket
    (IndexName "_dont_index_")
    parseError

  where
    parseError ::
         ByteString
      -> UnsetBucketIndexError
    parseError err
      | isBucketTypeDoesNotExistError4 err =
          BucketTypeDoesNotExistError bucketType
      | otherwise =
          UnknownError (decodeUtf8 err)

setBucketIndex_ ::
     forall op.
     Handle
  -> Bucket
  -> IndexName
  -> (ByteString -> Error op)
  -> IO (Either (Error op) ())
setBucketIndex_ handle bucket index parseError = liftIO $
  fromResult <$> Handle.setBucket handle request

  where
    request :: Proto.RpbSetBucketReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.props .~
            (Proto.defMessage
              & Proto.searchIndex .~ encodeUtf8 (_unIndexName index))

    fromResult ::
         Either [HandleError] (Either ByteString ())
      -> Either (Error op) ()
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseError err)

      Right (Right ()) ->
        Right ()

-- | Reset bucket properties.
resetBucket ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either ResetBucketError ())
resetBucket handle (Bucket bucketType bucket) = liftIO $
  fromResult <$> Handle.resetBucket handle request

  where
    request :: Proto.RpbResetBucketReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.type' .~ bucketType

    fromResult ::
         Either [HandleError] (Either ByteString ())
      -> Either ResetBucketError ()
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseError err)

      Right (Right _) ->
        Right ()

    parseError :: ByteString -> ResetBucketError
    parseError err =
      UnknownError (decodeUtf8 err)

-- | Perform a query on a binary secondary index.
--
-- +-------------------------------------+-------------------------------------+
-- | Error                               | Meaning                             |
-- +=====================================+=====================================+
-- | 'BucketTypeDoesNotExistError'       | The bucket type does not exist. You |
-- |                                     | must first create it using the      |
-- |                                     | @riak-admin@ command-line tool.     |
-- +-------------------------------------+-------------------------------------+
-- | 'SecondaryIndexesNotSupportedError' | The bucket's backend does not       |
-- |                                     | secondary indexes. Valid backends   |
-- |                                     | are @leveldb@ and @memory@.         |
-- +-------------------------------------+-------------------------------------+
queryBinaryIndex ::
     MonadIO m
  => Handle -- ^
  -> BinaryIndexQuery -- ^
  -> FoldM IO Key r -- ^
  -> m (Either QueryIndexError r)
queryBinaryIndex
    handle
    query@(BinaryIndexQuery { bucket, minValue, maxValue })
    keyFold = liftIO $

  doIndex
    handle
    bucket
    request
    (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      if minValue == maxValue
        then
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ BinaryIndexQuery.indexName query
            & Proto.key .~ minValue
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'eq
            & Proto.stream .~ True
        else
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ BinaryIndexQuery.indexName query
            & Proto.maxResults .~ 100
            & Proto.rangeMax .~ maxValue
            & Proto.rangeMin .~ minValue
            & Proto.qtype .~ Proto.RpbIndexReq'range
            & Proto.stream .~ True

    handler :: Foldl.HandlerM IO Proto.RpbIndexResp Key
    handler =
      Proto.keys . folded . to mkKey

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

-- | Perform a query on a binary secondary index.
--
-- +-------------------------------------+-------------------------------------+
-- | Error                               | Meaning                             |
-- +=====================================+=====================================+
-- | 'BucketTypeDoesNotExistError'       | The bucket type does not exist. You |
-- |                                     | must first create it using the      |
-- |                                     | @riak-admin@ command-line tool.     |
-- +-------------------------------------+-------------------------------------+
-- | 'SecondaryIndexesNotSupportedError' | The bucket's backend does not       |
-- |                                     | secondary indexes. Valid backends   |
-- |                                     | are @leveldb@ and @memory@.         |
-- +-------------------------------------+-------------------------------------+
queryBinaryIndexTerms ::
     MonadIO m
  => Handle -- ^
  -> BinaryIndexQuery -- ^
  -> FoldM IO (ByteString, Key) r -- ^
  -> m (Either QueryIndexError r)
queryBinaryIndexTerms
    handle
    BinaryIndexQuery { bucket, index, minValue, maxValue }
    keyFold = liftIO $

  doIndex
    handle
    bucket
    request
    (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      if index == builtinBucketIndex
        then
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ index
            & Proto.key .~ minValue
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'eq
            & Proto.stream .~ True
      else if index == builtinKeyIndex
        then
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ builtinKeyIndex
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'range
            & Proto.rangeMin .~ minValue
            & Proto.rangeMax .~ maxValue
            & Proto.returnTerms .~ True
            & Proto.stream .~ True
      else
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ (index <> "_bin")
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'range
            & Proto.rangeMin .~ minValue
            & Proto.rangeMax .~ maxValue
            & Proto.returnTerms .~ True
            & Proto.stream .~ True

    handler :: Foldl.HandlerM IO Proto.RpbIndexResp (ByteString, Key)
    handler =
      if index == builtinBucketIndex
        then
          Proto.keys .
          folded .
          to (\key -> (b, Key bucketType b key))
        else
          Proto.results .
          folded .
          to (\pair -> (pair ^. Proto.key, Key bucketType b (pair ^. Proto.value)))

    Bucket bucketType b =
      bucket

-- | Perform a query on an integer secondary index.
--
-- +-------------------------------------+-------------------------------------+
-- | Error                               | Meaning                             |
-- +=====================================+=====================================+
-- | 'BucketTypeDoesNotExistError'       | The bucket type does not exist. You |
-- |                                     | must first create it using the      |
-- |                                     | @riak-admin@ command-line tool.     |
-- +-------------------------------------+-------------------------------------+
-- | 'SecondaryIndexesNotSupportedError' | The bucket's backend does not       |
-- |                                     | secondary indexes. Valid backends   |
-- |                                     | are @leveldb@ and @memory@.         |
-- +-------------------------------------+-------------------------------------+
queryIntIndex ::
     MonadIO m
  => Handle -- ^
  -> IntIndexQuery -- ^
  -> FoldM IO Key r -- ^
  -> m (Either QueryIndexError r)
queryIntIndex handle IntIndexQuery { bucket, index, minValue, maxValue } keyFold = liftIO $
  doIndex
    handle
    bucket
    request
    (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      if minValue == maxValue
        then
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ (index <> "_int")
            & Proto.key .~ int2bs minValue
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'eq
            & Proto.stream .~ True
        else
          Proto.defMessage
            & setProto bucket
            & Proto.index .~ (index <> "_int")
            & Proto.maxResults .~ 100
            & Proto.qtype .~ Proto.RpbIndexReq'range
            & Proto.rangeMax .~ int2bs maxValue
            & Proto.rangeMin .~ int2bs minValue
            & Proto.stream .~ True

    handler :: Foldl.HandlerM IO Proto.RpbIndexResp Key
    handler =
      Proto.keys . folded . to mkKey

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

-- | Perform a query on an integer secondary index.
--
-- +-------------------------------------+-------------------------------------+
-- | Error                               | Meaning                             |
-- +=====================================+=====================================+
-- | 'BucketTypeDoesNotExistError'       | The bucket type does not exist. You |
-- |                                     | must first create it using the      |
-- |                                     | @riak-admin@ command-line tool.     |
-- +-------------------------------------+-------------------------------------+
-- | 'SecondaryIndexesNotSupportedError' | The bucket's backend does not       |
-- |                                     | secondary indexes. Valid backends   |
-- |                                     | are @leveldb@ and @memory@.         |
-- +-------------------------------------+-------------------------------------+
queryIntIndexTerms ::
     MonadIO m
  => Handle -- ^
  -> IntIndexQuery -- ^
  -> FoldM IO (Int64, Key) r -- ^
  -> m (Either QueryIndexError r)
queryIntIndexTerms
    handle
    IntIndexQuery { bucket, index, minValue, maxValue }
    keyFold = liftIO $

  doIndex
    handle
    bucket
    request
    (Foldl.handlesM handler keyFold)

  where
    request :: Proto.RpbIndexReq
    request =
      Proto.defMessage
        & setProto bucket
        & Proto.index .~ (index <> "_int")
        & Proto.maxResults .~ 100
        & Proto.rangeMax .~ int2bs maxValue
        & Proto.rangeMin .~ int2bs minValue
        & Proto.returnTerms .~ True
        & Proto.qtype .~ Proto.RpbIndexReq'range
        & Proto.stream .~ True

    handler :: Foldl.HandlerM IO Proto.RpbIndexResp (Int64, Key)
    handler =
      Proto.results . folded . to fromResult

    fromResult :: Proto.RpbPair -> (Int64, Key)
    fromResult pair =
      (bs2int (pair ^. Proto.key), mkKey (pair ^. Proto.value))

    mkKey :: ByteString -> Key
    mkKey =
      case bucket of
        Bucket bucketType bucket ->
          Key bucketType bucket

doIndex ::
     forall r.
     Handle
  -> Bucket
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either (Error 'SecondaryIndexQueryOp) r)
doIndex handle bucket@(Bucket bucketType _) =
  loop

  where
    loop ::
         Proto.RpbIndexReq
      -> FoldM IO Proto.RpbIndexResp r
      -> IO (Either (Error 'SecondaryIndexQueryOp) r)
    loop request responseFold = do
      result :: Either [HandleError] (Either ByteString (FoldM IO Proto.RpbIndexResp r, Maybe ByteString)) <-
        doIndexPage
          handle
          request
          (Foldl.duplicateM responseFold)

      case result of
        Left err ->
          pure (Left (HandleError err))

        Right (Left err) ->
          pure (Left (parseError err))

        Right (Right (nextResponseFold, continuation)) ->
          case continuation of
            Nothing ->
              Right <$> extractM nextResponseFold

            Just continuation -> do
              loop
                (request & Proto.continuation .~ continuation)
                nextResponseFold

    parseError ::
         ByteString
      -> Error 'SecondaryIndexQueryOp
    parseError err
      | isBucketTypeDoesNotExistError5 err =
          BucketTypeDoesNotExistError bucketType
      | isSecondaryIndexesNotSupportedError err =
          SecondaryIndexesNotSupportedError bucket
      | otherwise =
          UnknownError (decodeUtf8 err)


doIndexPage ::
     Handle
  -> Proto.RpbIndexReq
  -> FoldM IO Proto.RpbIndexResp r
  -> IO (Either [HandleError] (Either ByteString (r, Maybe ByteString)))
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
        & lmap (\response -> response ^. Proto.maybe'continuation)
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
-- 'Riak.SecondaryIndexQuery.inBucket' query.
--
-- /See also/: 'streamKeys'
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
listKeys ::
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> m (Either ListKeysError [Key])
listKeys handle bucket =
  streamKeys handle bucket (Foldl.generalize Foldl.list)

-- | Stream all of the keys in a bucket.
--
-- /Note/: This is an extremely expensive operation, and should not be used on a
-- production cluster.
--
-- /Note/: If your backend supports secondary indexes, it is faster to use the
-- 'Riak.SecondaryIndexQuery.inBucket' query.
--
-- /See also/: 'listKeys'
--
-- +-------------------------------+-------------------------------------------+
-- | Error                         | Meaning                                   |
-- +===============================+===========================================+
-- | 'BucketTypeDoesNotExistError' | The bucket type does not exist. You must  |
-- |                               | first create it using the @riak-admin@    |
-- |                               | command-line tool.                        |
-- +-------------------------------+-------------------------------------------+
streamKeys ::
     forall m r.
     MonadIO m
  => Handle -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> m (Either ListKeysError r)
streamKeys handle b@(Bucket bucketType bucket) keyFold = liftIO $
  fromResult <$> doRequest

  where
    doRequest :: IO (Either [HandleError] (Either ByteString r))
    doRequest =
      Handle.listKeys
        handle
        request
        (Foldl.handlesM
          (Proto.keys . folded . to (Key bucketType bucket))
          keyFold)

    fromResult ::
         Either [HandleError] (Either ByteString r)
      -> Either ListKeysError r
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseError err)

      Right (Right response) ->
        Right response

    request :: Proto.RpbListKeysReq
    request =
      Proto.defMessage
        & setProto b
        -- TODO stream keys timeout

    parseError :: ByteString -> ListKeysError
    parseError err
      | isBucketTypeDoesNotExistError4 err =
          BucketTypeDoesNotExistError bucketType
      | otherwise =
          UnknownError (decodeUtf8 err)

fromProto ::
     ( HasLens' a "bucket" ByteString
     , HasLens' a "type'" ByteString
     )
  => a
  -> Bucket
fromProto proto =
  Bucket
    (BucketType.fromProto proto)
    (proto ^. Proto.bucket)

setProto ::
     ( HasLens' a "bucket" ByteString
     , HasLens' a "maybe'type'" (Maybe ByteString)
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
