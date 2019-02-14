module Riak.ConvergentCounter
  ( ConvergentCounter(..)
  , getConvergentCounter
  , updateConvergentCounter
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

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
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.key .~ key
        & Proto.type' .~ bucketType

        -- TODO get counter opts
        -- & Proto.maybe'basicQuorum .~ undefined
        -- & Proto.maybe'nVal .~ undefined
        -- & Proto.maybe'notfoundOk .~ undefined
        -- & Proto.maybe'pr .~ undefined
        -- & Proto.maybe'r .~ undefined
        -- & Proto.maybe'sloppyQuorum .~ undefined
        -- & Proto.maybe'timeout .~ undefined

    fromResponse :: Proto.DtFetchResp -> Maybe ConvergentCounter
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentCounter
        { key = k
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
  -> m (Either Handle.Error ConvergentCounter)
updateConvergentCounter handle (ConvergentCounter { key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.updateCrdt handle request)

  where
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & Proto.op .~
            (Proto.defMessage
              & Proto.counterOp .~
                  (Proto.defMessage
                    & Proto.increment .~ value))
        & Proto.returnBody .~ True
        & Proto.type' .~ bucketType

-- TODO counter update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    fromResponse :: Proto.DtUpdateResp -> ConvergentCounter
    fromResponse response =
      ConvergentCounter
        { key =
            if ByteString.null k
              then Key bucketType bucket (response ^. Proto.key)
              else key
        , value =
            response ^. Proto.counterValue
        }
