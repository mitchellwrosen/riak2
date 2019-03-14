module RiakCounter
  ( ConvergentCounter(..)
  , getCounter
  , updateCounter
  ) where

import RiakCrdt
import RiakError
import RiakGetOpts (GetOpts)
import RiakHandle  (Handle)
import RiakKey     (Key(..))
import RiakPutOpts (PutOpts)
import RiakUtils   (retrying)

import qualified RiakGetOpts as GetOpts
import qualified RiakHandle  as Handle
import qualified RiakKey     as Key
import qualified RiakPutOpts as PutOpts

import Control.Lens ((.~), (^.))

import qualified Data.Riak.Proto as Proto


-- | An eventually-convergent counter.
--
-- Counters must be stored in a bucket type with the __@datatype = counter@__
-- property.
--
-- /Note/: Counters do not contain a causal context, so it is not necessary to
-- read a counter before updating it.
data ConvergentCounter
  = ConvergentCounter
  { key :: Key -- ^
  , value :: Int64 -- ^
  } deriving stock (Generic, Show)

-- | Get an eventually-convergent counter.
getCounter ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetCounterError (Maybe ConvergentCounter))
getCounter handle key opts =
  liftIO (retrying 1000000 (getCounter_ handle key opts))

getCounter_ ::
     Handle
  -> Key
  -> GetOpts
  -> IO (Maybe (Either GetCounterError (Maybe ConvergentCounter)))
getCounter_ handle key@(Key bucketType _ _) opts =

  Handle.getCrdt handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseGetCrdtError bucketType err)

    Right (Right response) ->
      pure (Just (Right (fromResponse response)))

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & GetOpts.setProto opts
        & Key.setProto key

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
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateCounter ::
     MonadIO m
  => Handle -- ^
  -> ConvergentCounter -- ^ Counter update
  -> PutOpts -- ^
  -> m (Either UpdateCounterError ConvergentCounter)
updateCounter handle counter opts =
  liftIO (retrying 1000000 (updateCounter_ handle counter opts))

updateCounter_ ::
     Handle
  -> ConvergentCounter
  -> PutOpts
  -> IO (Maybe (Either UpdateCounterError ConvergentCounter))
updateCounter_
    handle
    (ConvergentCounter key@(Key bucketType _ _) value)
    opts =

  Handle.updateCrdt handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseUpdateCrdtError bucketType err)

    Right (Right response) ->
      pure (Just (Right (fromResponse response)))

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
