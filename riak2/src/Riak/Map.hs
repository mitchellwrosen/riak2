module Riak.Map
  ( Map(..)
  , Maps(..)
  , getMap
  , updateMap
  , MapUpdate(..)
  ) where

import Riak.Client           (Client)
import Riak.Context          (Context)
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude
import Riak.Internal.Set     (SetUpdate)
import Riak.Key              (Key(..))

import qualified Riak.Interface    as Interface
import qualified Riak.Internal.Set as Set
import qualified Riak.Proto        as Proto
import qualified Riak.Proto.Lens   as L

import Data.Monoid (Endo(..))

import qualified Data.ByteString     as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet


-- | A map data type.
--
-- Maps must be stored in a bucket type with the __@datatype = set@__ property.
data Map a
  = Map
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^ Key
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | The map data.
--
-- In Riak, map values are uniquely keyed by both a name and type (it is
-- possible to have both a counter and a flag at key @"foo"@, for example).
data Maps
  = Maps
  { counters :: !(HashMap ByteString Int64) -- ^ Counters
  , flags :: !(HashMap ByteString Bool) -- ^ Flags
  , maps :: !(HashMap ByteString Maps) -- ^ Maps
  , registers :: !(HashMap ByteString ByteString) -- ^ Registers
  , sets :: !(HashMap ByteString (HashSet ByteString)) -- ^ Sets
  } deriving stock (Generic, Show)

-- | Left-biased union.
instance Monoid Maps where
  mempty = Maps mempty mempty mempty mempty mempty
  mappend = (<>)

-- | Left-biased union.
instance Semigroup Maps where
  Maps a1 b1 c1 d1 e1 <> Maps a2 b2 c2 d2 e2 =
    Maps (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

-- | A map update.
data MapUpdate
  = RemoveCounter ByteString
  | RemoveFlag ByteString
  | RemoveMap ByteString
  | RemoveRegister ByteString
  | RemoveSet ByteString
  | UpdateCounter ByteString Int64
  | UpdateFlag ByteString Bool
  | UpdateMap ByteString [MapUpdate]
  | UpdateRegister ByteString ByteString
  | UpdateSet ByteString [SetUpdate]
  deriving stock (Eq, Show)

-- | Get a map.
getMap ::
     MonadIO m
  => Client -- ^
  -> Key -- ^
  -> m (Either ByteString (Maybe (Map Maps)))
getMap client k@(Key bucketType bucket key) = liftIO $
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

        -- TODO get map opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: Proto.GetCrdtResponse -> Maybe (Map Maps)
    fromResponse response = do
      crdt :: Proto.Crdt <-
        response ^. L.maybe'value
      pure Map
        { context = Context (response ^. L.context)
        , key = k
        , value = mapValuesToMaps (crdt ^. L.map)
        }

-- | Update a map.
--
-- /See also/: @Riak.Context.'Riak.Context.newContext'@, @Riak.Key.'Riak.Key.generatedKey'@
updateMap ::
     MonadIO m
  => Client -- ^
  -> Map [MapUpdate] -- ^
  -> m (Either ByteString (Map Maps))
updateMap client (Map { context, key, value }) = liftIO $
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
              & L.mapUpdate .~ updatesToProto value)
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

    fromResponse :: Proto.UpdateCrdtResponse -> Map Maps
    fromResponse response =
      Map
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then Key bucketType bucket (response ^. L.key)
              else key
        , value = mapValuesToMaps (response ^. L.map)
        }

mapValuesToMaps :: [Proto.MapValue] -> Maps
mapValuesToMaps =
  foldMap mapValueToMaps

mapValueToMaps :: Proto.MapValue -> Maps
mapValueToMaps entry =
  case entry ^. L.field . L.type' of
    Proto.MapKey'COUNTER ->
      mempty { counters = HashMap.singleton name (entry ^. L.counter) }

    Proto.MapKey'FLAG ->
      mempty { flags = HashMap.singleton name (entry ^. L.flag) }

    Proto.MapKey'MAP ->
      mempty { maps = HashMap.singleton name (mapValuesToMaps (entry ^. L.map)) }

    Proto.MapKey'REGISTER ->
      mempty { registers = HashMap.singleton name (entry ^. L.register) }

    Proto.MapKey'SET ->
      mempty { sets = HashMap.singleton name (HashSet.fromList (entry ^. L.set)) }

  where
    name :: ByteString
    name =
      entry ^. L.field . L.name

updatesToProto :: [MapUpdate] -> Proto.MapUpdate
updatesToProto =
  ($ defMessage) . appEndo . foldMap (coerce updateToEndoProto)

updateToEndoProto :: MapUpdate -> Proto.MapUpdate -> Proto.MapUpdate
updateToEndoProto = \case
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
        defMessage
          & L.field .~ mapkey name Proto.MapKey'COUNTER
          & L.counterUpdate .~ (defMessage & L.increment .~ value)
    in
      L.updates %~ (update :)

  UpdateFlag name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        defMessage
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
        defMessage
          & L.field .~ mapkey name Proto.MapKey'MAP
          & L.mapUpdate .~ updatesToProto value
    in
      L.updates %~ (update :)

  UpdateRegister name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        defMessage
          & L.field .~ mapkey name Proto.MapKey'REGISTER
          & L.registerUpdate .~ value
    in
      L.updates %~ (update :)

  UpdateSet name value ->
    let
      update :: Proto.MapValueUpdate
      update =
        defMessage
          & L.field .~ mapkey name Proto.MapKey'SET
          & L.setUpdate .~ Set.updatesToProto value
    in
      L.updates %~ (update :)

  where
    mapkey :: ByteString -> Proto.MapKey'MapKeyType -> Proto.MapKey
    mapkey name type' =
      defMessage
        & L.name .~ name
        & L.type' .~ type'
