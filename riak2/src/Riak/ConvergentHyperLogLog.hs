-- |
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <http://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>
module Riak.ConvergentHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
  ) where

import Riak.Handle           (Handle)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Handle     as Handle
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

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
  -> m (Either ByteString (Maybe (ConvergentHyperLogLog Word64)))
getConvergentHyperLogLog handle k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get hll opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse ::
         Proto.GetCrdtResponse
      -> Maybe (ConvergentHyperLogLog Word64)
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure ConvergentHyperLogLog
        { key = k
        , value = crdt ^. L.hll
        }

-- | Update a HyperLogLog.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
updateConvergentHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> ConvergentHyperLogLog [ByteString] -- ^
  -> m (Either ByteString (ConvergentHyperLogLog Word64))
updateConvergentHyperLogLog handle (ConvergentHyperLogLog { key, value }) = liftIO $
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
            (Proto.defMessage
              & L.hllUpdate .~
                  (Proto.defMessage
                    & L.adds .~ value))
        & L.returnBody .~ True

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    fromResponse :: Proto.UpdateCrdtResponse -> ConvergentHyperLogLog Word64
    fromResponse response =
      ConvergentHyperLogLog
        { key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = response ^. L.hll
        }
