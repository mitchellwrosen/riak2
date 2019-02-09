module Riak.ConvergentCounter
  ( ConvergentCounter(..)
  , getConvergentCounter
  , updateConvergentCounter
  ) where

import Riak.Handle           (Handle)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Handle     as Handle
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Control.Lens ((.~), (^.))

import qualified ByteString


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
  -> m (Either Handle.Error (Maybe ConvergentCounter))
getConvergentCounter handle k@(Key bucketType bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.getCrdt handle request)

  where
    request :: Proto.GetCrdtRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.key .~ key

        -- TODO get counter opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: Proto.GetCrdtResponse -> Maybe ConvergentCounter
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure ConvergentCounter
        { key = k
        , value = crdt ^. L.counter
        }

-- | Update a counter.
--
-- /Note/: Counters, unlike other data types, represent their own update
-- operation.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
updateConvergentCounter ::
     MonadIO m
  => Handle -- ^
  -> ConvergentCounter -- ^
  -> m (Either Handle.Error ConvergentCounter)
updateConvergentCounter handle (ConvergentCounter { key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.updateCrdt handle request)

  where
    request :: Proto.UpdateCrdtRequest
    request =
      Proto.defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.update .~
            -- Missing value defaults to 1, so don't bother sending it
            case value of
              1 ->
                Proto.defMessage

              _ ->
                Proto.defMessage
                  & L.counterUpdate .~
                      (Proto.defMessage
                        & L.increment .~ value)
        & L.returnBody .~ True
-- TODO counter update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    fromResponse :: Proto.UpdateCrdtResponse -> ConvergentCounter
    fromResponse response =
      ConvergentCounter
        { key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value =
            response ^. L.counter
        }
