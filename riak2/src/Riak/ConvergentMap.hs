module Riak.ConvergentMap
  ( ConvergentMap(..)
  , ConvergentMapValue(..)
  , getConvergentMap
  , updateConvergentMap
  , ConvergentMapUpdate(..)
  ) where

import Riak.Context                (Context)
import Riak.Handle                 (Handle)
import Riak.Internal.Context       (Context(..))
import Riak.Internal.ConvergentSet (ConvergentSetUpdate)
import Riak.Internal.Prelude
import Riak.Key                    (Key(..))

import qualified Riak.Handle                 as Handle
import qualified Riak.Internal.ConvergentSet as ConvergentSet
import qualified Riak.Proto                  as Proto
import qualified Riak.Proto.Lens             as L

import Control.Lens ((%~), (.~), (^.))
import Data.Monoid  (Endo(..))

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
  -> m (Either ByteString (Maybe (ConvergentMap ConvergentMapValue)))
getConvergentMap handle k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get map opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse ::
         Proto.GetCrdtResponse
      -> Maybe (ConvergentMap ConvergentMapValue)
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure ConvergentMap
        { context = Context (response ^. L.context)
        , key = k
        , value = fromProtoValues (crdt ^. L.map)
        }

-- | Update a map.
--
-- /See also/: Riak.Context.'Riak.Context.newContext', Riak.Key.'Riak.Key.generatedKey'
updateConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> ConvergentMap [ConvergentMapUpdate] -- ^
  -> m (Either ByteString (ConvergentMap ConvergentMapValue))
updateConvergentMap handle (ConvergentMap { context, key, value }) = liftIO $
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
              & L.mapUpdate .~ toProtoUpdate value)
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

    fromResponse :: Proto.UpdateCrdtResponse -> ConvergentMap ConvergentMapValue
    fromResponse response =
      ConvergentMap
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = fromProtoValues (response ^. L.map)
        }

fromProtoValues :: [Proto.MapValue] -> ConvergentMapValue
fromProtoValues =
  foldMap fromProtoValue

fromProtoValue :: Proto.MapValue -> ConvergentMapValue
fromProtoValue entry =
  case entry ^. L.field . L.type' of
    Proto.MapKey'COUNTER ->
      mempty { counters = HashMap.singleton name (entry ^. L.counter) }

    Proto.MapKey'FLAG ->
      mempty { flags = HashMap.singleton name (entry ^. L.flag) }

    Proto.MapKey'MAP ->
      mempty { maps = HashMap.singleton name (fromProtoValues (entry ^. L.map)) }

    Proto.MapKey'REGISTER ->
      mempty { registers = HashMap.singleton name (entry ^. L.register) }

    Proto.MapKey'SET ->
      mempty { sets = HashMap.singleton name (HashSet.fromList (entry ^. L.set)) }

  where
    name :: ByteString
    name =
      entry ^. L.field . L.name

toProtoUpdate :: [ConvergentMapUpdate] -> Proto.MapUpdate
toProtoUpdate =
  ($ Proto.defMessage) . appEndo . foldMap (coerce toEndoProtoUpdate)

toEndoProtoUpdate :: ConvergentMapUpdate -> Proto.MapUpdate -> Proto.MapUpdate
toEndoProtoUpdate = \case
  RemoveCounter name ->
    L.removes %~ (mapkey name Proto.MapKey'COUNTER :)

  RemoveFlag name ->
    L.removes %~ (mapkey name Proto.MapKey'FLAG :)

  RemoveMap name ->
    L.removes %~ (mapkey name Proto.MapKey'MAP :)

  RemoveRegister name ->
    L.removes %~ (mapkey name Proto.MapKey'REGISTER :)

  RemoveSet name ->
    L.removes %~ (mapkey name Proto.MapKey'SET :)

  UpdateCounter name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        Proto.defMessage
          & L.field .~ mapkey name Proto.MapKey'COUNTER
          & L.counterUpdate .~ (Proto.defMessage & L.increment .~ value)
    in
      L.updates %~ (update :)

  UpdateFlag name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        Proto.defMessage
          & L.field .~ mapkey name Proto.MapKey'FLAG
          & L.flagUpdate .~
              case value of
                False -> Proto.MapValueUpdate'DISABLE
                True  -> Proto.MapValueUpdate'ENABLE
    in
      L.updates %~ (update :)

  UpdateMap name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        Proto.defMessage
          & L.field .~ mapkey name Proto.MapKey'MAP
          & L.mapUpdate .~ toProtoUpdate value
    in
      L.updates %~ (update :)

  UpdateRegister name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        Proto.defMessage
          & L.field .~ mapkey name Proto.MapKey'REGISTER
          & L.registerUpdate .~ value
    in
      L.updates %~ (update :)

  UpdateSet name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        Proto.defMessage
          & L.field .~ mapkey name Proto.MapKey'SET
          & L.setUpdate .~ ConvergentSet.toProtoUpdate value
    in
      L.updates %~ (update :)

  where
    mapkey :: ByteString -> Proto.MapKey'MapKeyType -> Proto.MapKey
    mapkey name type' =
      Proto.defMessage
        & L.name .~ name
        & L.type' .~ type'
