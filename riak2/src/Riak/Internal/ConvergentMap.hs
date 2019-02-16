module Riak.Internal.ConvergentMap where

import Libriak.Handle              (Handle)
import Riak.Context                (Context)
import Riak.Internal.Context       (Context(..), newContext)
import Riak.Internal.ConvergentSet (calculateSetOp)
import Riak.Internal.Crdt          (parseGetCrdtError, parseUpdateCrdtError)
import Riak.Internal.Error
import Riak.Internal.Key           (Key(..), isGeneratedKey)
import Riak.Internal.Prelude

import qualified Libriak.Handle    as Handle
import qualified Libriak.Proto     as Proto
import qualified Riak.Internal.Key as Key

import Control.Lens          (Lens', (%~), (.~), (^.))
import Data.Generics.Product (field)
import Data.List             (foldl')
import Data.Maybe            (mapMaybe)

import qualified Data.ByteString     as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet


-- | An eventually-convergent map.
--
-- Maps must be stored in a bucket type with the __@datatype = map@__ property.
data ConvergentMap a
  = ConvergentMap
  { _context :: !Context
  , _key :: !Key
  , _newValue :: !a
  , _oldValue :: !a
  } deriving stock (Functor, Generic, Show)

-- | Create a new convergent map.
newConvergentMap ::
     Key -- ^
  -> ConvergentMapValue -- ^
  -> ConvergentMap ConvergentMapValue
newConvergentMap key value =
  ConvergentMap
    { _context = newContext
    , _key = key
    , _newValue = value
    , _oldValue = emptyConvergentMapValue
    }

-- | A lens onto the key of a convergent map.
convergentMapKey :: Lens' (ConvergentMap a) Key
convergentMapKey =
  field @"_key"

-- | A lens onto the value of a convergent map.
convergentMapValue :: Lens' (ConvergentMap a) a
convergentMapValue =
  field @"_newValue"

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

-- | An empty convergent map value.
emptyConvergentMapValue :: ConvergentMapValue
emptyConvergentMapValue =
  ConvergentMapValue
    { counters = HashMap.empty
    , flags = HashMap.empty
    , maps = HashMap.empty
    , registers = HashMap.empty
    , sets = HashMap.empty
    }

-- | Get a convergent map.
getConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either (Error 'GetCrdtOp) (Maybe (ConvergentMap ConvergentMapValue)))
getConvergentMap handle key@(Key bucketType _ _) = liftIO $
  bimap (parseGetCrdtError bucketType) fromResponse <$>
    Handle.getCrdt handle request

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & Key.setProto key

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

      let
        value :: ConvergentMapValue
        value =
          fromProtoMapEntries (crdt ^. Proto.mapValue)

      pure ConvergentMap
        { _context = Context (response ^. Proto.context)
        , _key = key
        , _newValue = value
        , _oldValue = value
        }

-- | Put a convergent map.
putConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> ConvergentMap ConvergentMapValue -- ^
  -> m (Either (Error 'UpdateCrdtOp) (ConvergentMap ConvergentMapValue))
putConvergentMap
    handle
    (ConvergentMap context key@(Key bucketType _ _) newValue oldValue) = liftIO $
  bimap (parseUpdateCrdtError bucketType) fromResponse <$>
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
              & Proto.mapOp .~ calculateMapOp newValue oldValue)
        & Proto.returnBody .~ True

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    fromResponse :: Proto.DtUpdateResp -> ConvergentMap ConvergentMapValue
    fromResponse response =
      ConvergentMap
        { _context = Context (response ^. Proto.context)
        , _key =
            if isGeneratedKey key
              then
                case key of
                  Key bucketType bucket _ ->
                    Key bucketType bucket (response ^. Proto.key)
              else
                key
        , _newValue = value
        , _oldValue = value
        }

      where
        value :: ConvergentMapValue
        value =
          fromProtoMapEntries (response ^. Proto.mapValue)

fromProtoMapEntries :: [Proto.MapEntry] -> ConvergentMapValue
fromProtoMapEntries =
  foldl' step emptyConvergentMapValue

  where
    step :: ConvergentMapValue -> Proto.MapEntry -> ConvergentMapValue
    step acc entry =
      fromProtoMapEntry entry acc

fromProtoMapEntry :: Proto.MapEntry -> ConvergentMapValue -> ConvergentMapValue
fromProtoMapEntry entry =
  case entry ^. Proto.field . Proto.type' of
    Proto.MapField'COUNTER ->
      field @"counters" %~
        HashMap.insert name (entry ^. Proto.counterValue)

    Proto.MapField'FLAG ->
      field @"flags" %~
        (HashMap.insert name (entry ^. Proto.flagValue))

    Proto.MapField'MAP ->
      field @"maps" %~
        (HashMap.insert name (fromProtoMapEntries (entry ^. Proto.mapValue)))

    Proto.MapField'REGISTER ->
      field @"registers" %~
        (HashMap.insert name (entry ^. Proto.registerValue))

    Proto.MapField'SET ->
      field @"sets" %~
        (HashMap.insert name (HashSet.fromList (entry ^. Proto.setValue)))

  where
    name :: ByteString
    name =
      entry ^. Proto.field . Proto.name

calculateMapOp :: ConvergentMapValue -> ConvergentMapValue -> Proto.MapOp
calculateMapOp newValue oldValue =
  Proto.defMessage
    & Proto.removes .~ removes
    & Proto.updates .~ updates

  where
    removes :: [Proto.MapField]
    removes =
      concat
        [ makeRemoves counters  Proto.MapField'COUNTER
        , makeRemoves flags     Proto.MapField'FLAG
        , makeRemoves maps      Proto.MapField'MAP
        , makeRemoves registers Proto.MapField'REGISTER
        , makeRemoves sets      Proto.MapField'SET
        ]

      where
        makeRemoves ::
            (ConvergentMapValue -> HashMap ByteString a)
          -> Proto.MapField'MapFieldType
          -> [Proto.MapField]
        makeRemoves f t =
          HashMap.difference (f oldValue) (f newValue)
            & HashMap.keys
            & map
                (\key ->
                  Proto.defMessage
                    & Proto.name .~ key
                    & Proto.type' .~ t)


    updates :: [Proto.MapUpdate]
    updates =
      concat
        [ counterUpdates
        , flagUpdates
        , mapUpdates
        , registerUpdates
        , setUpdates
        ]

      where
        counterUpdates :: [Proto.MapUpdate]
        counterUpdates =
          mapMaybe
            (\(key, newValue) ->
              case HashMap.lookup key oldCounters of
                Nothing ->
                  Just (counterUpdate key newValue)
                Just oldValue ->
                  case newValue - oldValue of
                    0 ->
                      Nothing
                    difference ->
                      Just (counterUpdate key difference))
            (HashMap.toList (counters newValue))

          where
            oldCounters :: HashMap ByteString Int64
            oldCounters =
              counters oldValue

        flagUpdates :: [Proto.MapUpdate]
        flagUpdates =
          mapMaybe
            (\(key, newValue) ->
              case HashMap.lookup key oldFlags of
                Nothing ->
                  Just (flagUpdate key newValue)
                Just oldValue -> do
                  guard (newValue /= oldValue)
                  Just (flagUpdate key newValue))
            (HashMap.toList (flags newValue))

          where
            oldFlags :: HashMap ByteString Bool
            oldFlags =
              flags oldValue

        mapUpdates :: [Proto.MapUpdate]
        mapUpdates =
          mapMaybe
            (\(key, newValue) ->
              let
                op :: Proto.MapOp
                op =
                  calculateMapOp
                    newValue
                    (fromMaybe
                      emptyConvergentMapValue
                      (HashMap.lookup key oldMaps))
              in do
                guard (not (isEmptyMapOp op))
                Just (mapUpdate key op))
            (HashMap.toList (maps newValue))

          where
            oldMaps :: HashMap ByteString ConvergentMapValue
            oldMaps =
              maps oldValue

        registerUpdates :: [Proto.MapUpdate]
        registerUpdates =
          mapMaybe
            (\(key, newValue) ->
              case HashMap.lookup key oldRegisters of
                Nothing ->
                  Just (registerUpdate key newValue)
                Just oldValue -> do
                  guard (newValue /= oldValue)
                  Just (registerUpdate key newValue))
            (HashMap.toList (registers newValue))

          where
            oldRegisters :: HashMap ByteString ByteString
            oldRegisters =
              registers oldValue

        setUpdates :: [Proto.MapUpdate]
        setUpdates =
          mapMaybe
            (\(key, newValue) ->
              let
                op :: Proto.SetOp
                op =
                  calculateSetOp
                    newValue
                    (fromMaybe HashSet.empty (HashMap.lookup key oldSets))
              in do
                guard (not (isEmptySetOp op))
                Just (setUpdate key op))
            (HashMap.toList (sets newValue))

          where
            oldSets :: HashMap ByteString (HashSet ByteString)
            oldSets =
              sets oldValue

counterUpdate :: ByteString -> Int64 -> Proto.MapUpdate
counterUpdate key value =
  Proto.defMessage
    & Proto.field .~
        (Proto.defMessage
          & Proto.name .~ key
          & Proto.type' .~ Proto.MapField'COUNTER)
    & Proto.counterOp .~
        (Proto.defMessage
          & Proto.increment .~ value)

flagUpdate :: ByteString -> Bool -> Proto.MapUpdate
flagUpdate key value =
  Proto.defMessage
    & Proto.field .~
        (Proto.defMessage
          & Proto.name .~ key
          & Proto.type' .~ Proto.MapField'FLAG)
    & Proto.flagOp .~
        (if value
          then Proto.MapUpdate'ENABLE
          else Proto.MapUpdate'DISABLE)

mapUpdate :: ByteString -> Proto.MapOp -> Proto.MapUpdate
mapUpdate key value =
  Proto.defMessage
    & Proto.field .~
        (Proto.defMessage
          & Proto.name .~ key
          & Proto.type' .~ Proto.MapField'MAP)
    & Proto.mapOp .~
        value

registerUpdate :: ByteString -> ByteString -> Proto.MapUpdate
registerUpdate key value =
  Proto.defMessage
    & Proto.field .~
        (Proto.defMessage
          & Proto.name .~ key
          & Proto.type' .~ Proto.MapField'REGISTER)
    & Proto.registerOp .~
        value

setUpdate :: ByteString -> Proto.SetOp -> Proto.MapUpdate
setUpdate key value =
  Proto.defMessage
    & Proto.field .~
        (Proto.defMessage
          & Proto.name .~ key
          & Proto.type' .~ Proto.MapField'SET)
    & Proto.setOp .~
        value

isEmptyMapOp :: Proto.MapOp -> Bool
isEmptyMapOp op =
  null (op ^. Proto.removes) && null (op ^. Proto.updates)

isEmptySetOp :: Proto.SetOp -> Bool
isEmptySetOp op =
  null (op ^. Proto.adds) && null (op ^. Proto.removes)
