module Riak.Map
  ( Map(..)
  , Maps(..)
  , get
  , update
  , Update(..)
  ) where

import Riak.Context          (Context)
import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude
import Riak.Key              (Key(..))
import Riak.Proto

import qualified Riak.Interface    as Interface
import qualified Riak.Internal.Set as Set
import qualified Riak.Proto.Lens   as L

import Data.Monoid (Endo(..))

import qualified Data.ByteString     as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet


-- | A map data type.
--
-- A map is parameterized by the value contained within, so the same data
-- structure can be used for reading and modifying.
data Map a
  = Map
  { context :: !Context -- ^ Causal context
  , key :: !Key -- ^ Key
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

data Maps
  = Maps
  { counters :: !(HashMap ByteString Int64) -- ^ Counters
  , flags :: !(HashMap ByteString Bool) -- ^ Flags
  , maps :: !(HashMap ByteString Maps) -- ^ Maps
  , registers :: !(HashMap ByteString ByteString) -- ^ Registers
  , sets :: !(HashMap ByteString (HashSet ByteString)) -- ^ Sets
  } deriving stock (Generic, Show)

instance Monoid Maps where
  mempty = Maps mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup Maps where
  Maps a1 b1 c1 d1 e1 <> Maps a2 b2 c2 d2 e2 =
    Maps (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

-- | A map update.
data Update
  = RemoveCounter ByteString
  | RemoveFlag ByteString
  | RemoveMap ByteString
  | RemoveRegister ByteString
  | RemoveSet ByteString
  | UpdateCounter ByteString Int64
  | UpdateFlag ByteString Bool
  | UpdateMap ByteString [Update]
  | UpdateRegister ByteString ByteString
  | UpdateSet ByteString [Set.Update]
  deriving stock (Eq, Show)

-- | Get a map.
get ::
     MonadIO m
  => Client
  -> Key
  -> m (Result (Map Maps))
get client k@(Key type' bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt (iface client) request)

  where
    request :: DtFetchReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.includeContext .~ True
        & L.key .~ key
        & L.type' .~ type'

        -- TODO get map opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: DtFetchResp -> Map Maps
    fromResponse response =
      Map
        { context = Context (response ^. L.context)
        , key = k
        , value = mapEntriesToMaps (response ^. L.value . L.mapValue)
        }

-- | Update a map.
--
-- To update a map for the first time, use an empty causal context:
--
-- @
-- 'Map'
--   { context = Riak.Context.'Riak.Context.none'
--   , key = ...
--   , value = ...
--   }
-- @
--
-- Otherwise, you must 'get' a map before you 'update' it.
update ::
     MonadIO m
  => Client
  -> Map [Update]
  -> m (Result (Map Maps))
update client (Map { context, key, value }) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt (iface client) request)

  where
    request :: DtUpdateReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.includeContext .~ True
        & L.maybe'context .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.op .~
            (defMessage
              & L.mapOp .~ updatesToOp value)
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

    fromResponse :: DtUpdateResp -> Map Maps
    fromResponse response =
      Map
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then key { key = response ^. L.key }
              else key
        , value = mapEntriesToMaps (response ^. L.mapValue)
        }

mapEntriesToMaps :: [MapEntry] -> Maps
mapEntriesToMaps =
  foldMap mapEntryToMaps

mapEntryToMaps :: MapEntry -> Maps
mapEntryToMaps entry =
  case entry ^. L.field . L.type' of
    MapField'COUNTER ->
      mempty { counters = HashMap.singleton name (entry ^. L.counterValue) }

    MapField'FLAG ->
      mempty { flags = HashMap.singleton name (entry ^. L.flagValue) }

    MapField'MAP ->
      mempty { maps = HashMap.singleton name (mapEntriesToMaps (entry ^. L.mapValue)) }

    MapField'REGISTER ->
      mempty { registers = HashMap.singleton name (entry ^. L.registerValue) }

    MapField'SET ->
      mempty { sets = HashMap.singleton name (HashSet.fromList (entry ^. L.setValue)) }

  where
    name :: ByteString
    name =
      entry ^. L.field . L.name

updatesToOp :: [Update] -> MapOp
updatesToOp =
  ($ defMessage) . appEndo . foldMap (coerce updateToEndoOp)

updateToEndoOp :: Update -> MapOp -> MapOp
updateToEndoOp = \case
  RemoveCounter name ->
    L.removes %~ (mapfield name MapField'COUNTER :)

  RemoveFlag name ->
    L.removes %~ (mapfield name MapField'FLAG :)

  RemoveMap name ->
    L.removes %~ (mapfield name MapField'MAP :)

  RemoveRegister name ->
    L.removes %~ (mapfield name MapField'REGISTER :)

  RemoveSet name ->
    L.removes %~ (mapfield name MapField'SET :)

  UpdateCounter name value ->
    let
      update :: MapUpdate
      update =
        defMessage
          & L.field .~ mapfield name MapField'COUNTER
          & L.counterOp .~ (defMessage & L.increment .~ value)
    in
      L.updates %~ (update :)

  UpdateFlag name value ->
    let
      update :: MapUpdate
      update =
        defMessage
          & L.field .~ mapfield name MapField'FLAG
          & L.flagOp .~
              case value of
                False -> MapUpdate'DISABLE
                True  -> MapUpdate'ENABLE
    in
      L.updates %~ (update :)

  UpdateMap name value ->
    let
      update :: MapUpdate
      update =
        defMessage
          & L.field .~ mapfield name MapField'MAP
          & L.mapOp .~ updatesToOp value
    in
      L.updates %~ (update :)

  UpdateRegister name value ->
    let
      update :: MapUpdate
      update =
        defMessage
          & L.field .~ mapfield name MapField'REGISTER
          & L.registerOp .~ value
    in
      L.updates %~ (update :)

  UpdateSet name value ->
    let
      update :: MapUpdate
      update =
        defMessage
          & L.field .~ mapfield name MapField'SET
          & L.setOp .~ Set.updatesToOp value
    in
      L.updates %~ (update :)

  where
    mapfield :: ByteString -> MapField'MapFieldType -> MapField
    mapfield name type' =
      defMessage
        & L.name .~ name
        & L.type' .~ type'
