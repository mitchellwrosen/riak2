-- |
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <http://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>
module Riak.ConvergentHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Libriak.Handle     as Handle
import qualified Libriak.Proto      as Proto

import Control.Lens ((.~), (^.))

import qualified ByteString


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
  { key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | Get a HyperLogLog.
getConvergentHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either Handle.Error (Maybe (ConvergentHyperLogLog Word64)))
getConvergentHyperLogLog handle k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get hll opts
        -- & Proto.maybe'basicQuorum .~ undefined
        -- & Proto.maybe'nVal .~ undefined
        -- & Proto.maybe'notfoundOk .~ undefined
        -- & Proto.maybe'pr .~ undefined
        -- & Proto.maybe'r .~ undefined
        -- & Proto.maybe'sloppyQuorum .~ undefined
        -- & Proto.maybe'timeout .~ undefined

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentHyperLogLog Word64)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentHyperLogLog
        { key = k
        , value = crdt ^. Proto.hllValue
        }

-- | Update a HyperLogLog.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateConvergentHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> ConvergentHyperLogLog [ByteString] -- ^
  -> m (Either Handle.Error (ConvergentHyperLogLog Word64))
updateConvergentHyperLogLog handle (ConvergentHyperLogLog { key, value }) = liftIO $
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
              & Proto.hllOp .~
                  (Proto.defMessage
                    & Proto.adds .~ value))
        & Proto.returnBody .~ True
        & Proto.type' .~ bucketType

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    fromResponse :: Proto.DtUpdateResp -> ConvergentHyperLogLog Word64
    fromResponse response =
      ConvergentHyperLogLog
        { key =
            if ByteString.null k
              then Key bucketType bucket (response ^. Proto.key)
              else key
        , value = response ^. Proto.hllValue
        }
