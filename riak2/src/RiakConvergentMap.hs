module RiakConvergentMap
  ( getConvergentMap
  , putConvergentMap
    -- * Convergent map
  , ConvergentMap
  , newConvergentMap
  , convergentMapKey
  , convergentMapValue
  ) where

import Libriak.Handle         (Handle)
import RiakContext            (Context(..), newContext)
import RiakConvergentMapValue (ConvergentMapValue(..), emptyConvergentMapValue)
import RiakCrdt               (parseGetCrdtError, parseUpdateCrdtError)
import RiakError
import RiakKey                (Key(..), isGeneratedKey)
import RiakUtils              (retrying)

import qualified Libriak.Handle         as Handle
import qualified Libriak.Proto          as Proto
import qualified RiakConvergentMapValue as ConvergentMapValue
import qualified RiakKey                as Key

import Control.Lens          (Lens', (.~), (^.))
import Data.Generics.Product (field)

import qualified Data.ByteString as ByteString


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

-- | Get a convergent map.
getConvergentMap ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either GetConvergentMapError (Maybe (ConvergentMap ConvergentMapValue)))
getConvergentMap handle key =
  liftIO (retrying 1000000 (getConvergentMap_ handle key))

getConvergentMap_ ::
     Handle
  -> Key
  -> IO (Maybe (Either GetConvergentMapError (Maybe (ConvergentMap ConvergentMapValue))))
getConvergentMap_ handle key@(Key bucketType _ _) =
  Handle.getCrdt handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseGetCrdtError bucketType err)

    Right (Right response) ->
      pure (Just (Right (fromResponse response)))

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
          ConvergentMapValue.fromProto (crdt ^. Proto.mapValue)

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
  -> m (Either PutConvergentMapError (ConvergentMap ConvergentMapValue))
putConvergentMap handle value =
  liftIO (retrying 1000000 (putConvergentMap_ handle value))

putConvergentMap_ ::
     Handle -- ^
  -> ConvergentMap ConvergentMapValue -- ^
  -> IO (Maybe (Either PutConvergentMapError (ConvergentMap ConvergentMapValue)))
putConvergentMap_
    handle
    (ConvergentMap context key@(Key bucketType _ _) newValue oldValue) =

  Handle.updateCrdt handle request >>= \case
    Left err ->
      pure (Just (Left (HandleError err)))

    Right (Left err) ->
      pure (Left <$> parseUpdateCrdtError bucketType err)

    Right (Right response) ->
      pure (Just (Right (fromResponse response)))

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
              & Proto.mapOp .~ ConvergentMapValue.toProto newValue oldValue)
        & Proto.returnBody .~ True

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    fromResponse ::
         Proto.DtUpdateResp
      -> ConvergentMap ConvergentMapValue
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
          ConvergentMapValue.fromProto (response ^. Proto.mapValue)
