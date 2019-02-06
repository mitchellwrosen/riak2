-- |
-- * <http://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <http://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>
module Riak.HyperLogLog
  ( HyperLogLog(..)
  , getHyperLogLog
  , updateHyperLogLog
  ) where

import Riak.Internal.Client  (Client)
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Interface as Interface
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L

import qualified Data.ByteString as ByteString


-- | A HyperLogLog data type, which provides an approximate cardinality of a
-- set.
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
data HyperLogLog a
  = HyperLogLog
  { key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | Get a HyperLogLog.
getHyperLogLog ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Either ByteString (Maybe (HyperLogLog Word64)))
getHyperLogLog client k@(Key bucketType bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt client request)

  where
    request :: Proto.GetCrdtRequest
    request =
      defMessage
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

    fromResponse :: Proto.GetCrdtResponse -> Maybe (HyperLogLog Word64)
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure HyperLogLog
        { key = k
        , value = crdt ^. L.hll
        }

-- | Update a HyperLogLog.
--
-- /See also/: Riak.Key.'Riak.Key.none'
updateHyperLogLog ::
     MonadIO m
  => Client -- ^
  -> HyperLogLog [ByteString] -- ^
  -> m (Either ByteString (HyperLogLog Word64))
updateHyperLogLog client (HyperLogLog { key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt client request)

  where
    request :: Proto.UpdateCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.update .~
            (defMessage
              & L.hllUpdate .~
                  (defMessage
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

    fromResponse :: Proto.UpdateCrdtResponse -> HyperLogLog Word64
    fromResponse response =
      HyperLogLog
        { key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = response ^. L.hll
        }
