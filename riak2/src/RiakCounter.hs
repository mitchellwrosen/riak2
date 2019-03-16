module RiakCounter
  ( ConvergentCounter(..)
  , getCounter
  , updateCounter
  ) where

import RiakCrdt
import RiakError
import RiakGetOpts (GetOpts)
import RiakHandle  (Handle)
import RiakKey     (keyBucket)
import RiakKey     (Key(..))
import RiakPutOpts (PutOpts)

import qualified RiakGetOpts as GetOpts
import qualified RiakHandle  as Handle
import qualified RiakKey     as Key
import qualified RiakPutOpts as PutOpts

import Control.Lens       ((.~), (^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto

-- | An eventually-convergent counter.
--
-- Counters must be stored in a bucket type with the @datatype = counter@
-- property.
--
-- /Note/: Counters do not contain a causal context, so it is not necessary to
-- read a counter before updating it.
data ConvergentCounter
  = ConvergentCounter
  { key :: Key -- ^ Key (read-only)
  , value :: Int64 -- ^ Value (read-write)
  } deriving stock (Eq, Generic, Show)

-- | Get an eventually-convergent counter.
getCounter ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetCounterError (Maybe ConvergentCounter))
getCounter handle key@(Key bucketType _ _) opts = liftIO $
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
      -> Maybe ConvergentCounter
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentCounter
        { key = key
        , value = crdt ^. Proto.counterValue
        }

-- | Update an eventually-convergent counter.
--
-- /Note/: Counters, unlike other convergent data types, represent their own
-- update operation.
updateCounter ::
     MonadIO m
  => Handle -- ^
  -> ConvergentCounter -- ^ Counter update
  -> PutOpts -- ^
  -> m (Either UpdateCounterError ConvergentCounter)
updateCounter handle (ConvergentCounter key@(Key bucketType _ _) value) opts = liftIO $
  fromResult <$>
    Handle.updateCrdt handle request

  where
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Key.setMaybeProto key
        & Proto.op .~
            (Proto.defMessage
              & Proto.counterOp .~
                  (Proto.defMessage
                    & Proto.increment .~ value))
        & Proto.returnBody .~ True
        & PutOpts.setProto opts

    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseError err)

      Right (Right response) ->
        Right (fromResponse response)

    fromResponse ::
         Proto.DtUpdateResp
      -> ConvergentCounter
    fromResponse response =
      ConvergentCounter
        { key =
            if Key.isGeneratedKey key
              then
                case key of
                  Key bucketType bucket _ ->
                    Key bucketType bucket (response ^. Proto.key)
              else
                key
        , value =
            response ^. Proto.counterValue
        }

    parseError ::
         ByteString
      -> Error 'UpdateCrdtOp
    parseError err
      | isBucketTypeDoesNotExistError1 err =
          BucketTypeDoesNotExistError bucketType
      | isBucketMustBeAllowMultError err =
          InvalidBucketError (key ^. keyBucket)
      | isInvalidCounterBucketError err =
          InvalidBucketError (key ^. keyBucket)
      | isInvalidNodesError0 err =
          InvalidNodesError
      | isOperationTypeIsCounterButBucketTypeIsError err =
          InvalidBucketTypeError bucketType
      | otherwise =
          UnknownError (decodeUtf8 err)
