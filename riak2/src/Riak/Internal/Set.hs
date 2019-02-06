module Riak.Internal.Set where

import Riak.Client           (Client)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude hiding (Set)
import Riak.Key              (Key(..))

import qualified Riak.Interface  as Interface
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import qualified Data.ByteString as ByteString
import qualified Data.HashSet    as HashSet


-- | A set data type.
--
-- Sets must be stored in a bucket type with the __@datatype = set@__ property.
data Set a
  = Set
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | A set update.
data SetUpdate
  = Add ByteString
  | Remove ByteString
  deriving stock (Eq, Show)

-- | Get a set.
getSet ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Either ByteString (Maybe (Set (HashSet ByteString))))
getSet client k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get set opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: Proto.GetCrdtResponse -> Maybe (Set (HashSet ByteString))
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure Set
        { context = Context (response ^. L.context)
        , key = k
        , value = HashSet.fromList (crdt ^. L.set)
        }

-- | Update a set.
--
-- /See also/: @Riak.Context.'Riak.Context.none'@, @Riak.Key.'Riak.Key.none'@
updateSet ::
     MonadIO m
  => Client -- ^
  -> Set [SetUpdate] -- ^
  -> m (Either ByteString (Set (HashSet ByteString)))
updateSet client (Set { context, key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt client request)

  where
    request :: Proto.UpdateCrdtRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.bucketType .~ bucketType
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
              & L.setUpdate .~ updatesToProto value)
        & L.returnBody .~ True

-- TODO set update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    fromResponse :: Proto.UpdateCrdtResponse -> Set (HashSet ByteString)
    fromResponse response =
      Set
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = HashSet.fromList (response ^. L.set)
        }

updatesToProto :: [SetUpdate] -> Proto.SetUpdate
updatesToProto updates =
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
