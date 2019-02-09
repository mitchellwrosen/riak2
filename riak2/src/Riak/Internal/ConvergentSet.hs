module Riak.Internal.ConvergentSet where

import Riak.Handle           (Handle)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Riak.Handle     as Handle
import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Control.Lens ((.~), (^.))

import qualified ByteString
import qualified HashSet


-- | An eventually-convergent set.
--
-- Sets must be stored in a bucket type with the __@datatype = set@__ property.
data ConvergentSet a
  = ConvergentSet
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | A set update.
data ConvergentSetUpdate
  = Add ByteString
  | Remove ByteString
  deriving stock (Eq, Show)

-- | Get a set.
getConvergentSet ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either Handle.Error (Maybe (ConvergentSet (HashSet ByteString))))
getConvergentSet handle k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get set opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse ::
         Proto.GetCrdtResponse
      -> Maybe (ConvergentSet (HashSet ByteString))
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure ConvergentSet
        { context = Context (response ^. L.context)
        , key = k
        , value = HashSet.fromList (crdt ^. L.set)
        }

-- | Update a set.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
updateConvergentSet ::
     MonadIO m
  => Handle -- ^
  -> ConvergentSet [ConvergentSetUpdate] -- ^
  -> m (Either Handle.Error (ConvergentSet (HashSet ByteString)))
updateConvergentSet handle (ConvergentSet { context, key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Handle.updateCrdt handle request)

  where
    request :: Proto.UpdateCrdtRequest
    request =
      Proto.defMessage
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
            (Proto.defMessage
              & L.setUpdate .~ toProtoUpdate value)
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

    fromResponse ::
         Proto.UpdateCrdtResponse
      -> ConvergentSet (HashSet ByteString)
    fromResponse response =
      ConvergentSet
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = HashSet.fromList (response ^. L.set)
        }

toProtoUpdate :: [ConvergentSetUpdate] -> Proto.SetUpdate
toProtoUpdate updates =
  Proto.defMessage
    & L.adds .~ adds
    & L.removes .~ removes

  where
    adds :: [ByteString]
    adds =
      [ value | Add value <- updates ]

    removes :: [ByteString]
    removes =
      [ value | Remove value <- updates ]
