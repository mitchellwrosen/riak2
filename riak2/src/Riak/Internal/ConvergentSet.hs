module Riak.Internal.ConvergentSet where

import Libriak.Handle        (Handle)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))

import qualified Libriak.Handle as Handle
import qualified Libriak.Proto  as Proto

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
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.key .~ key
        & Proto.type' .~ bucketType

        -- TODO get set opts
        -- & Proto.maybe'basicQuorum .~ undefined
        -- & Proto.maybe'nVal .~ undefined
        -- & Proto.maybe'notfoundOk .~ undefined
        -- & Proto.maybe'pr .~ undefined
        -- & Proto.maybe'r .~ undefined
        -- & Proto.maybe'sloppyQuorum .~ undefined
        -- & Proto.maybe'timeout .~ undefined

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentSet (HashSet ByteString))
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentSet
        { context = Context (response ^. Proto.context)
        , key = k
        , value = HashSet.fromList (crdt ^. Proto.setValue)
        }

-- | Update a set.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
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
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Proto.bucket .~ bucket
        & Proto.maybe'context .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & Proto.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & Proto.op .~
            (Proto.defMessage
              & Proto.setOp .~ toProtoSetOp value)
        & Proto.returnBody .~ True
        & Proto.type' .~ bucketType

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
         Proto.DtUpdateResp
      -> ConvergentSet (HashSet ByteString)
    fromResponse response =
      ConvergentSet
        { context = Context (response ^. Proto.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. Proto.key)
              else key
        , value = HashSet.fromList (response ^. Proto.setValue)
        }

toProtoSetOp :: [ConvergentSetUpdate] -> Proto.SetOp
toProtoSetOp updates =
  Proto.defMessage
    & Proto.adds .~ adds
    & Proto.removes .~ removes

  where
    adds :: [ByteString]
    adds =
      [ value | Add value <- updates ]

    removes :: [ByteString]
    removes =
      [ value | Remove value <- updates ]
