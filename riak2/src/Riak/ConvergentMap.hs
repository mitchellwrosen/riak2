module Riak.ConvergentMap
  ( ConvergentMap(..)
  , ConvergentMapValue(..)
  , getConvergentMap
  , updateConvergentMap
  , ConvergentMapUpdate(..)
  ) where

import Libriak.Handle              (Handle)
import Riak.Context                (Context)
import Riak.Internal.Context       (Context(..))
import Riak.Internal.ConvergentSet (ConvergentSetUpdate)
import Riak.Internal.Error
import Riak.Internal.Prelude
import Riak.Key                    (Key(..))

import qualified Libriak.Handle              as Handle
import qualified Libriak.Proto               as Proto
import qualified Riak.Internal.ConvergentSet as ConvergentSet
import qualified Riak.Internal.Key           as Key

import Control.Lens       ((%~), (.~), (^.))
import Data.Monoid        (Endo(..))
import Data.Text.Encoding (decodeUtf8)

import qualified ByteString
import qualified HashMap
import qualified HashSet


-- | An eventually-convergent map.
--
-- Maps must be stored in a bucket type with the __@datatype = map@__ property.
data ConvergentMap a
  = ConvergentMap
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^ Key
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | The map data.
--
-- In Riak, map values are uniquely keyed by both a name and type (it is
-- possible to have both a counter and a flag at key @"foo"@, for example).
data ConvergentMapValue
  = ConvergentMapValue
  { counters :: !(HashMap ByteString Int64) -- ^ Counters
  , flags :: !(HashMap ByteString Bool) -- ^ Flags
  , maps :: !(HashMap ByteString ConvergentMapValue) -- ^ Maps
  , registers :: !(HashMap ByteString ByteString) -- ^ Registers
  , sets :: !(HashMap ByteString (HashSet ByteString)) -- ^ Sets
  } deriving stock (Generic, Show)

-- | Left-biased union.
instance Monoid ConvergentMapValue where
  mempty = ConvergentMapValue mempty mempty mempty mempty mempty
  mappend = (<>)

-- | Left-biased union.
instance Semigroup ConvergentMapValue where
  ConvergentMapValue a1 b1 c1 d1 e1 <> ConvergentMapValue a2 b2 c2 d2 e2 =
    ConvergentMapValue (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

-- | A map update.
data ConvergentMapUpdate
  = RemoveCounter ByteString
  | RemoveFlag ByteString
  | RemoveMap ByteString
  | RemoveRegister ByteString
  | RemoveSet ByteString
  | UpdateCounter ByteString Int64
  | UpdateFlag ByteString Bool
  | UpdateMap ByteString [ConvergentMapUpdate]
  | UpdateRegister ByteString ByteString
  | UpdateSet ByteString [ConvergentSetUpdate]
  deriving stock (Eq, Show)


-- | Get a map.
getConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either Handle.Error (Maybe (ConvergentMap ConvergentMapValue)))
getConvergentMap handle k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get map opts
        -- & Proto.maybe'basicQuorum .~ undefined
        -- & Proto.maybe'nVal .~ undefined
        -- & Proto.maybe'notfoundOk .~ undefined
        -- & Proto.maybe'pr .~ undefined
        -- & Proto.maybe'r .~ undefined
        -- & Proto.maybe'sloppyQuorum .~ undefined
        -- & Proto.maybe'timeout .~ undefined

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentMap ConvergentMapValue)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value
      pure ConvergentMap
        { context = Context (response ^. Proto.context)
        , key = k
        , value = fromProtoMapEntries (crdt ^. Proto.mapValue)
        }

-- | Update a map.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> ConvergentMap [ConvergentMapUpdate] -- ^
  -> m (Either (Error 'UpdateMapOp) (ConvergentMap ConvergentMapValue))
updateConvergentMap handle (ConvergentMap { context, key, value }) = liftIO $
  bimap parseUpdateMapError fromResponse <$>
    Handle.updateCrdt handle request

  where
    request :: Proto.DtUpdateReq
    request =
      Proto.defMessage
        & Key.setMaybeProto key
        & Proto.maybe'context .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & Proto.op .~
            (Proto.defMessage
              & Proto.mapOp .~ toProtoMapOp value)
        & Proto.returnBody .~ True

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key bucketType bucket k =
      key

    parseUpdateMapError :: Handle.Error -> Error 'UpdateMapOp
    parseUpdateMapError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | Just err' <- isMapFieldDoesNotExistError err ->
            MapFieldDoesNotExistError err'
        | otherwise ->
            UnknownError (decodeUtf8 err)

    fromResponse :: Proto.DtUpdateResp -> ConvergentMap ConvergentMapValue
    fromResponse response =
      ConvergentMap
        { context = Context (response ^. Proto.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. Proto.key)
              else key
        , value = fromProtoMapEntries (response ^. Proto.mapValue)
        }

fromProtoMapEntries :: [Proto.MapEntry] -> ConvergentMapValue
fromProtoMapEntries =
  foldMap fromProtoMapEntry

fromProtoMapEntry :: Proto.MapEntry -> ConvergentMapValue
fromProtoMapEntry entry =
  case entry ^. Proto.field . Proto.type' of
    Proto.MapField'COUNTER ->
      mempty { counters = HashMap.singleton name (entry ^. Proto.counterValue) }

    Proto.MapField'FLAG ->
      mempty { flags = HashMap.singleton name (entry ^. Proto.flagValue) }

    Proto.MapField'MAP ->
      mempty { maps = HashMap.singleton name (fromProtoMapEntries (entry ^. Proto.mapValue)) }

    Proto.MapField'REGISTER ->
      mempty { registers = HashMap.singleton name (entry ^. Proto.registerValue) }

    Proto.MapField'SET ->
      mempty { sets = HashMap.singleton name (HashSet.fromList (entry ^. Proto.setValue)) }

  where
    name :: ByteString
    name =
      entry ^. Proto.field . Proto.name

toProtoMapOp :: [ConvergentMapUpdate] -> Proto.MapOp
toProtoMapOp =
  ($ Proto.defMessage) . appEndo . foldMap (coerce toEndoProtoMapOp)

toEndoProtoMapOp :: ConvergentMapUpdate -> Proto.MapOp -> Proto.MapOp
toEndoProtoMapOp = \case
  RemoveCounter name ->
    Proto.removes %~ (mapfield name Proto.MapField'COUNTER :)

  RemoveFlag name ->
    Proto.removes %~ (mapfield name Proto.MapField'FLAG :)

  RemoveMap name ->
    Proto.removes %~ (mapfield name Proto.MapField'MAP :)

  RemoveRegister name ->
    Proto.removes %~ (mapfield name Proto.MapField'REGISTER :)

  RemoveSet name ->
    Proto.removes %~ (mapfield name Proto.MapField'SET :)

  UpdateCounter name value ->
    let
      update :: Proto.MapUpdate
      update =
        Proto.defMessage
          & Proto.field .~ mapfield name Proto.MapField'COUNTER
          & Proto.counterOp .~ (Proto.defMessage & Proto.increment .~ value)
    in
      Proto.updates %~ (update :)

  UpdateFlag name value ->
    let
      update :: Proto.MapUpdate
      update =
        Proto.defMessage
          & Proto.field .~ mapfield name Proto.MapField'FLAG
          & Proto.flagOp .~
              case value of
                False -> Proto.MapUpdate'DISABLE
                True  -> Proto.MapUpdate'ENABLE
    in
      Proto.updates %~ (update :)

  UpdateMap name value ->
    let
      update :: Proto.MapUpdate
      update =
        Proto.defMessage
          & Proto.field .~ mapfield name Proto.MapField'MAP
          & Proto.mapOp .~ toProtoMapOp value
    in
      Proto.updates %~ (update :)

  UpdateRegister name value ->
    let
      update :: Proto.MapUpdate
      update =
        Proto.defMessage
          & Proto.field .~ mapfield name Proto.MapField'REGISTER
          & Proto.registerOp .~ value
    in
      Proto.updates %~ (update :)

  UpdateSet name value ->
    let
      update :: Proto.MapUpdate
      update =
        Proto.defMessage
          & Proto.field .~ mapfield name Proto.MapField'SET
          & Proto.setOp .~ ConvergentSet.toProtoSetOp value
    in
      Proto.updates %~ (update :)

  where
    mapfield :: ByteString -> Proto.MapField'MapFieldType -> Proto.MapField
    mapfield name type' =
      Proto.defMessage
        & Proto.name .~ name
        & Proto.type' .~ type'
