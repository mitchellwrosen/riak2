module RiakMap
  ( getMap
  , putMap
    -- * Convergent map
  , ConvergentMap
  , newMap
  , mapKey
  , mapValue
  ) where

import RiakContext     (Context(..), newContext)
import RiakCrdt        (parseGetCrdtError, parseUpdateCrdtError)
import RiakError
import RiakGetOpts     (GetOpts)
import RiakHandle      (Handle)
import RiakHandleError (HandleError)
import RiakKey         (Key(..), isGeneratedKey)
import RiakMapValue    (ConvergentMapValue(..), emptyMapValue)
import RiakPutOpts     (PutOpts)
import RiakUtils       (retrying)

import qualified RiakGetOpts  as GetOpts
import qualified RiakHandle   as Handle
import qualified RiakKey      as Key
import qualified RiakMapValue as MapValue
import qualified RiakPutOpts  as PutOpts

import Control.Lens          (Lens', (.~), (^.))
import Data.Generics.Product (field)

import qualified Data.ByteString as ByteString
import qualified Data.Riak.Proto as Proto


-- | An eventually-convergent map.
--
-- Maps must be stored in a bucket type with the __@datatype = map@__ property.
data ConvergentMap a
  = ConvergentMap
  { _context :: Context
  , _key :: Key
  , _newValue :: a
  , _oldValue :: a
  } deriving stock (Functor, Generic, Show)

-- | Create a new eventually-convergent map.
newMap ::
     Key -- ^
  -> ConvergentMapValue -- ^
  -> ConvergentMap ConvergentMapValue
newMap key value =
  ConvergentMap
    { _context = newContext
    , _key = key
    , _newValue = value
    , _oldValue = emptyMapValue
    }

-- | A lens onto the key of an eventually-convergent map.
mapKey :: Lens' (ConvergentMap a) Key
mapKey =
  field @"_key"

-- | A lens onto the value of an eventually-convergent map.
mapValue :: Lens' (ConvergentMap a) a
mapValue =
  field @"_newValue"

-- | Get an eventually-convergent map.
getMap ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> GetOpts -- ^
  -> m (Either GetMapError (Maybe (ConvergentMap ConvergentMapValue)))
getMap handle key opts =
  liftIO (retrying 1000000 (getMap_ handle key opts))

getMap_ ::
     Handle
  -> Key
  -> GetOpts
  -> IO (Maybe (Either GetMapError (Maybe (ConvergentMap ConvergentMapValue))))
getMap_ handle key@(Key bucketType _ _) opts =
  fromResult <$> Handle.getCrdt handle request

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & GetOpts.setProto opts
        & Key.setProto key

    fromResult ::
         Either HandleError (Either ByteString Proto.DtFetchResp)
      -> Maybe (Either GetMapError (Maybe (ConvergentMap ConvergentMapValue)))
    fromResult = \case
      Left err ->
        Just (Left (HandleError err))

      Right (Left err) ->
        Left <$> parseGetCrdtError bucketType err

      Right (Right response) ->
        Just (Right (fromResponse response))

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentMap ConvergentMapValue)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value

      let
        value :: ConvergentMapValue
        value =
          MapValue.fromProto (crdt ^. Proto.mapValue)

      pure ConvergentMap
        { _context = Context (response ^. Proto.context)
        , _key = key
        , _newValue = value
        , _oldValue = value
        }

-- | Put an eventually-convergent map.
putMap ::
     MonadIO m
  => Handle -- ^
  -> ConvergentMap ConvergentMapValue -- ^
  -> PutOpts
  -> m (Either PutMapError (ConvergentMap ConvergentMapValue))
putMap handle value opts =
  liftIO (retrying 1000000 (putMap_ handle value opts))

putMap_ ::
     Handle
  -> ConvergentMap ConvergentMapValue
  -> PutOpts
  -> IO (Maybe (Either PutMapError (ConvergentMap ConvergentMapValue)))
putMap_
    handle
    (ConvergentMap context key@(Key bucketType _ _) newValue oldValue)
    opts =

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
              & Proto.mapOp .~ MapValue.toProto newValue oldValue)
        & Proto.returnBody .~ True
        & PutOpts.setProto opts

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
          MapValue.fromProto (response ^. Proto.mapValue)
