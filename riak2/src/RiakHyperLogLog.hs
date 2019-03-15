-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <https://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>

module RiakHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getHyperLogLog
  , updateHyperLogLog
  ) where

import RiakBucket  (Bucket(..))
import RiakCrdt
import RiakError
import RiakGetOpts (GetOpts)
import RiakHandle  (Handle)
import RiakKey     (keyBucket)
import RiakKey     (Key(..), isGeneratedKey)
import RiakPutOpts (PutOpts)

import qualified RiakGetOpts     as GetOpts
import qualified RiakHandle      as Handle
import qualified RiakHandleError as HandleError
import qualified RiakKey         as Key
import qualified RiakPutOpts     as PutOpts

import Control.Lens       ((.~), (^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto


-- | An eventually-convergent HyperLogLog, which provides an approximate
-- cardinality of a set.
--
-- HyperLogLogs must be stored in a bucket type with the __@datatype = hll@__
-- property.
--
-- The @hllPrecision@ bucket type property controls the number of precision bits
-- to use. Valid values are 4-16 (inclusive), and the default value is 14. The
-- precision may only be decreased, never increased.
--
-- /Note/: HyperLogLogs do not contain a causal context, so it is not necessary
-- to read a HyperLogLog before updating it.
data ConvergentHyperLogLog a
  = ConvergentHyperLogLog
  { key :: Key -- ^
  , value :: a -- ^
  } deriving stock (Eq, Functor, Generic, Show)

-- | Get an eventually-convergent HyperLogLog.
getHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetHyperLogLogError (Maybe (ConvergentHyperLogLog Word64)))
getHyperLogLog handle key@(Key bucketType _ _) opts = liftIO $
  fromResult <$> Handle.getCrdt handle request

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & GetOpts.setProto opts
        & Key.setProto key

    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseGetCrdtError bucketType err)

      Right (Right response) ->
        Right (fromResponse response)

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentHyperLogLog Word64)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value

      pure ConvergentHyperLogLog
        { key = key
        , value = crdt ^. Proto.hllValue
        }

-- | Update an eventually-convergent HyperLogLog.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> ConvergentHyperLogLog [ByteString] -- ^
  -> PutOpts -- ^
  -> m (Either UpdateHyperLogLogError (ConvergentHyperLogLog Word64))
updateHyperLogLog handle (ConvergentHyperLogLog key value) opts = liftIO $

  fromResult <$> Handle.updateCrdt handle request

  where
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Key.setMaybeProto key
        & Proto.op .~
            (Proto.defMessage
              & Proto.hllOp .~
                  (Proto.defMessage
                    & Proto.adds .~ value))
        & Proto.returnBody .~ True
        & PutOpts.setProto opts

    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseUpdateHyperLogLogError (key ^. keyBucket) err)

      Right (Right response) ->
        Right (fromResponse response)

    fromResponse :: Proto.DtUpdateResp -> ConvergentHyperLogLog Word64
    fromResponse response =
      ConvergentHyperLogLog
        { key =
            if isGeneratedKey key
              then
                case key of
                  Key bucketType bucket _ ->
                    Key bucketType bucket (response ^. Proto.key)
              else
                key
        , value =
            response ^. Proto.hllValue
        }

parseUpdateHyperLogLogError ::
     Bucket
  -> ByteString
  -> Error 'UpdateCrdtOp
parseUpdateHyperLogLogError bucket@(Bucket bucketType _) err
  | isBucketMustBeAllowMultError err =
      InvalidBucketError bucket
  | isBucketTypeDoesNotExistError1 err =
      BucketTypeDoesNotExistError bucketType
  | isInvalidNodesError0 err =
      InvalidNodesError
  | isNonCounterOperationOnDefaultBucketError err =
      InvalidBucketTypeError bucketType
  | isOperationTypeIsHllButBucketTypeIsError err =
      InvalidBucketTypeError bucketType
  | isTimeoutError err =
      HandleError HandleError.HandleTimeoutError
  | otherwise =
      UnknownError (decodeUtf8 err)
