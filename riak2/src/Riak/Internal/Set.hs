module Riak.Internal.Set where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude hiding (Set)
import Riak.Key              (Key(..))
import Riak.Proto

import qualified Riak.Interface  as Interface
import qualified Riak.Proto.Lens as L

import qualified Data.ByteString as ByteString
import qualified Data.HashSet    as HashSet


-- | A set data type.
data Set a
  = Set
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | A set update.
data Update
  = Add ByteString
  | Remove ByteString
  deriving stock (Eq, Show)

-- | Get a set.
get ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Result (Set (HashSet ByteString)))
get client k@(Key type' bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt (iface client) request)

  where
    request :: GetCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.key .~ key
        & L.type' .~ type'

        -- TODO get set opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: GetCrdtResponse -> Set (HashSet ByteString)
    fromResponse response =
      Set
        { context = Context (response ^. L.context)
        , key = k
        , value = HashSet.fromList (response ^. L.value . L.setValue)
        }

-- | Update a set.
--
-- To update a set for the first time, use an empty causal context:
--
-- @
-- 'Set'
--   { context = Riak.Context.'Riak.Context.none'
--   , key = ...
--   , value = ...
--   }
-- @
--
-- Otherwise, you must 'get' a set before you 'update' it.
update ::
     MonadIO m
  => Client -- ^
  -> Set [Update] -- ^
  -> m (Result (Set (HashSet ByteString)))
update client (Set { context, key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt (iface client) request)

  where
    request :: UpdateCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.maybe'context .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.update .~
            (defMessage
              & L.set .~ updatesToSetUpdate value)
        & L.returnBody .~ True
        & L.type' .~ type'

-- TODO set update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key type' bucket k =
      key

    fromResponse :: UpdateCrdtResponse -> Set (HashSet ByteString)
    fromResponse response =
      Set
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then key { key = response ^. L.key }
              else key
        , value = HashSet.fromList (response ^. L.set)
        }

updatesToSetUpdate :: [Update] -> SetUpdate
updatesToSetUpdate updates =
  defMessage
    & L.adds .~ adds
    & L.removes .~ removes

  where
    adds :: [ByteString]
    adds =
      [ value | Add value <- updates ]

    removes :: [ByteString]
    removes =
      [ value | Remove value <- updates ]
