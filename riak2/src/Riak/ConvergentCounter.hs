module Riak.ConvergentCounter
  ( ConvergentCounter(..)
  , getConvergentCounter
  , updateConvergentCounter
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Object           (GetOpts(..), PutOpts(..))

import qualified Libriak.Handle       as Handle
import qualified Libriak.Proto        as Proto
import qualified Riak.Internal.Key    as Key
import qualified Riak.Internal.Quorum as Quorum

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
  -> m (Either Handle.Error (Maybe ConvergentCounter))
getConvergentCounter
    handle
    key
    (GetOpts basicQuorum nodes notfoundOk pr r timeout) = liftIO $

  (fmap.fmap)
    fromResponse
    (Handle.getCrdt handle request)

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & Key.setProto key
        & Proto.maybe'basicQuorum .~ basicQuorum
        & Proto.maybe'notfoundOk .~ notfoundOk
        & Proto.maybe'nVal .~ (Quorum.toWord32 <$> nodes)
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
  -> m (Either Handle.Error ConvergentCounter)
updateConvergentCounter
    handle
    (ConvergentCounter key value)
    (PutOpts dw nodes pw timeout w) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.updateCrdt handle request)

  where
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Key.setMaybeProto key
        & Proto.maybe'dw .~ (Quorum.toWord32 <$> dw)
        & Proto.maybe'nVal .~ (Quorum.toWord32 <$> nodes)
        & Proto.maybe'pw .~ (Quorum.toWord32 <$> pw)
        & Proto.maybe'timeout .~ timeout
        & Proto.maybe'w .~ (Quorum.toWord32 <$> w)
        & Proto.op .~
            (Proto.defMessage
              & Proto.counterOp .~
                  (Proto.defMessage
                    & Proto.increment .~ value))
        & Proto.returnBody .~ True

    fromResponse :: Proto.DtUpdateResp -> ConvergentCounter
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
