module Riak.HyperLogLog
  ( HyperLogLog(..)
  , get
  , update
  ) where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Interface  as Interface
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import qualified Data.ByteString as ByteString


-- | A HyperLogLog data type.
data HyperLogLog a
  = HyperLogLog
  { key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | Get a HyperLogLog.
get ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Result (HyperLogLog Word64))
get client k@(Key type' bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt (iface client) request)

  where
    request :: Proto.GetCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.key .~ key
        & L.type' .~ type'

        -- TODO get hll opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: Proto.GetCrdtResponse -> HyperLogLog Word64
    fromResponse response =
      HyperLogLog
        { key = k
        , value = response ^. L.value . L.hll
        }

-- | Update a HyperLogLog.
update ::
     MonadIO m
  => Client -- ^
  -> HyperLogLog [ByteString] -- ^
  -> m (Result (HyperLogLog Word64))
update client (HyperLogLog { key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt (iface client) request)

  where
    request :: Proto.UpdateCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.update .~
            (defMessage
              & L.hll .~
                  (defMessage
                    & L.adds .~ value))
        & L.returnBody .~ True
        & L.type' .~ type'

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key type' bucket k =
      key

    fromResponse :: Proto.UpdateCrdtResponse -> HyperLogLog Word64
    fromResponse response =
      HyperLogLog
        { key =
            if ByteString.null k
              then key { key = response ^. L.key }
              else key
        , value = response ^. L.hll
        }
