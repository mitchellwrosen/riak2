module RiakConvergentCounter
  ( ConvergentCounter(..)
  , getConvergentCounter
  , updateConvergentCounter
  ) where

import Libriak.Handle (Handle)
import RiakCrdt
import RiakError
import RiakKey        (Key(..))
import RiakObject     (GetOpts(..), PutOpts(..))
import RiakUtils      (retrying)

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto
import qualified RiakKey        as Key
import qualified RiakQuorum     as Quorum

import Control.Lens ((.~), (^.))


-- | An eventually-convergent counter.
--
-- Counters must be stored in a bucket type with the __@datatype = counter@__
-- property.
--
-- /Note/: Counters do not contain a causal context, so it is not necessary to
-- read a counter before updating it.
data ConvergentCounter
  = ConvergentCounter
  { key :: !Key -- ^
  , value :: !Int64 -- ^
  } deriving stock (Generic, Show)

-- | Get a counter.
getConvergentCounter ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetConvergentCounterError (Maybe ConvergentCounter))
getConvergentCounter handle key opts =
  liftIO (retrying 1000000 (getConvergentCounter_ handle key opts))

getConvergentCounter_ ::
     Handle
  -> Key
  -> GetOpts
  -> IO (Maybe (Either GetConvergentCounterError (Maybe ConvergentCounter)))
getConvergentCounter_
    handle
    key@(Key bucketType _ _)
    (GetOpts basicQuorum nodes notfoundOk pr r timeout) =

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
        & Key.setProto key
        & Proto.maybe'basicQuorum .~ basicQuorum
        & Proto.maybe'notfoundOk .~ notfoundOk
        & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
        & Proto.maybe'pr .~ (Quorum.toWord32 <$> pr)
        & Proto.maybe'r .~ (Quorum.toWord32 <$> r)
        & Proto.maybe'timeout .~ timeout

    fromResponse :: Proto.DtFetchResp -> Maybe ConvergentCounter
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentCounter
        { key = key
        , value = crdt ^. Proto.counterValue
        }

-- | Update a counter.
--
-- /Note/: Counters, unlike other data types, represent their own update
-- operation.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateConvergentCounter ::
     MonadIO m
  => Handle -- ^
  -> ConvergentCounter -- ^
  -> PutOpts -- ^
  -> m (Either UpdateConvergentCounterError ConvergentCounter)
updateConvergentCounter handle counter opts =
  liftIO (retrying 1000000 (updateConvergentCounter_ handle counter opts))

updateConvergentCounter_ ::
     Handle
  -> ConvergentCounter
  -> PutOpts
  -> IO (Maybe (Either UpdateConvergentCounterError ConvergentCounter))
updateConvergentCounter_
    handle
    (ConvergentCounter key@(Key bucketType _ _) value)
    (PutOpts dw nodes pw timeout w) =

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
        & Proto.maybe'dw .~ (Quorum.toWord32 <$> dw)
        & Proto.maybe'nVal .~ (fromIntegral <$> nodes)
        & Proto.maybe'pw .~ (Quorum.toWord32 <$> pw)
        & Proto.maybe'timeout .~ timeout
        & Proto.maybe'w .~ (Quorum.toWord32 <$> w)
        & Proto.op .~
            (Proto.defMessage
              & Proto.counterOp .~
                  (Proto.defMessage
                    & Proto.increment .~ value))
        & Proto.returnBody .~ True

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
