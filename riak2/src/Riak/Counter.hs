module Riak.Counter
  ( Counter(..)
  , get
  , update
  ) where

import Riak.Interface        (Result(..))
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Prelude
import Riak.Key              (Key)
import Riak.Key              (Key(..))
import Riak.Proto

import qualified Riak.Interface  as Interface
import qualified Riak.Proto.Lens as L

import qualified Data.ByteString as ByteString


-- | A counter data type.
--
-- Unlike other data types, a counter is _not_ parameterized by a type variable,
-- because reading and modifying a counter both carry an 'Int64'.
data Counter
  = Counter
  { key :: !Key
  , value :: !Int64
  } deriving stock (Generic, Show)

-- | Get a counter.
get ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Result Counter)
get client k@(Key type' bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt (iface client) request)

  where
    request :: DtFetchReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.key .~ key
        & L.type' .~ type'

        -- TODO get counter opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: DtFetchResp -> Counter
    fromResponse response =
      Counter
        { key = k
        , value = response ^. L.value . L.counterValue
        }

-- | Update a counter.
--
-- Note that counters, unlike other data types, represent their own update
-- operation. Furthermore, counters do not contain a causal context.
--
-- So to increment a counter at key __@K@__ by value __@N@__, you need not fetch
-- it first. Rather, just perform:
--
-- @
-- update client Counter { key = K, value = N }
-- @
update ::
     MonadIO m
  => Client
  -> Counter
  -> m (Result Counter)
update client (Counter { key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt (iface client) request)

  where
    request :: DtUpdateReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.op .~
            (defMessage
              & L.counterOp .~
                  (defMessage
                    & L.increment .~ value))
        & L.returnBody .~ True
        & L.type' .~ type'
-- TODO counter update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'includeContext :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key type' bucket k =
      key

    fromResponse :: DtUpdateResp -> Counter
    fromResponse response =
      Counter
        { key =
            if ByteString.null k
              then key { key = response ^. L.key }
              else key
        , value = response ^. L.counterValue
        }
