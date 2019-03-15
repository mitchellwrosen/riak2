module RiakMap
  ( getMap
  , putMap
    -- * Convergent map
  , ConvergentMap
  , newMap
  , mapKey
  , mapValue
  ) where

import RiakBucket      (Bucket(..))
import RiakContext     (Context(..), newContext)
import RiakCrdt        (parseGetCrdtError)
import RiakError
import RiakGetOpts     (GetOpts)
import RiakHandle      (Handle)
import RiakHandleError (HandleError)
import RiakKey         (Key(..), isGeneratedKey, keyBucket)
import RiakMapValue    (ConvergentMapValue(..), emptyMapValue)
import RiakPutOpts     (PutOpts)

import qualified RiakGetOpts     as GetOpts
import qualified RiakHandle      as Handle
import qualified RiakHandleError as HandleError
import qualified RiakKey         as Key
import qualified RiakMapValue    as MapValue
import qualified RiakPutOpts     as PutOpts

import Control.Lens          (Lens', (.~), (^.))
import Data.Generics.Product (field)
import Data.Text.Encoding    (decodeUtf8)

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
  } deriving stock (Eq, Functor, Generic, Show)

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
getMap handle key@(Key bucketType _ _) opts = liftIO $
  fromResult <$> Handle.getCrdt handle request

  where
    request :: Proto.DtFetchReq
    request =
      Proto.defMessage
        & GetOpts.setProto opts
        & Key.setProto key

    fromResult ::
         Either HandleError (Either ByteString Proto.DtFetchResp)
      -> Either GetMapError (Maybe (ConvergentMap ConvergentMapValue))
    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parseGetCrdtError bucketType err)

      Right (Right response) ->
        Right (fromResponse response)

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
putMap
    handle
    (ConvergentMap context key newValue oldValue)
    opts = liftIO $

  fromResult <$> Handle.updateCrdt handle request

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

    fromResult = \case
      Left err ->
        Left (HandleError err)

      Right (Left err) ->
        Left (parsePutMapError (key ^. keyBucket) err)

      Right (Right response) ->
        Right (fromResponse response)

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

parsePutMapError ::
     Bucket
  -> ByteString
  -> Error 'UpdateCrdtOp
parsePutMapError bucket@(Bucket bucketType _) err
  | isBucketMustBeAllowMultError err =
      InvalidBucketError bucket
  | isBucketTypeDoesNotExistError1 err =
      BucketTypeDoesNotExistError bucketType
  | isInvalidNodesError0 err =
      InvalidNodesError
  | isNonCounterOperationOnDefaultBucketError err =
      InvalidBucketTypeError bucketType
  | isOperationTypeIsMapButBucketTypeIsError err =
      InvalidBucketTypeError bucketType
  | isTimeoutError err =
      HandleError HandleError.HandleTimeoutError
  | otherwise =
      UnknownError (decodeUtf8 err)
