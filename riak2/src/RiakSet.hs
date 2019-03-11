module RiakSet
  ( ConvergentSet(..)
  , newSet
  , setKey
  , setValue
  , getSet
  , putSet
  , toProto
  ) where

import RiakContext (Context(..), newContext)
import RiakCrdt    (parseGetCrdtError, parseUpdateCrdtError)
import RiakError
import RiakHandle  (Handle)
import RiakKey     (Key(..), isGeneratedKey)
import RiakUtils   (retrying)

import qualified RiakHandle as Handle
import qualified RiakKey    as Key

import Control.Lens          (Lens', (.~), (^.))
import Data.Generics.Product (field)

import qualified Data.ByteString as ByteString
import qualified Data.HashSet    as HashSet
import qualified Data.Riak.Proto as Proto


-- | An eventually-convergent set.
--
-- Sets must be stored in a bucket type with the__@datatype = set@__ property.
data ConvergentSet a
  = ConvergentSet
  { _context :: !Context
  , _key :: !Key
  , _newValue :: !(HashSet a)
  , _oldValue :: !(HashSet a)
  } deriving stock (Generic, Show)

-- | Create a new eventually-convergent set.
newSet ::
     Key -- ^
  -> HashSet a -- ^
  -> ConvergentSet a
newSet key contents =
  ConvergentSet
    { _context = newContext
    , _key = key
    , _newValue = contents
    , _oldValue = HashSet.empty
    }

-- | A lens onto the key of an eventually-convergent set.
setKey :: Lens' (ConvergentSet a) Key
setKey =
  field @"_key"

-- | A lens onto the value of an eventually-convergent set.
setValue :: Lens' (ConvergentSet a) (HashSet a)
setValue =
  field @"_newValue"

-- | Get an eventually-convergent set.
getSet ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either GetSetError (Maybe (ConvergentSet ByteString)))
getSet handle key =
  liftIO (retrying 1000000 (getSet_ handle key))

getSet_ ::
     Handle
  -> Key
  -> IO (Maybe (Either GetSetError (Maybe (ConvergentSet ByteString))))
getSet_ handle key@(Key bucketType _ _) =
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
      -> Maybe (ConvergentSet ByteString)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value

      let
        value :: HashSet ByteString
        value =
          HashSet.fromList (crdt ^. Proto.setValue)

      pure ConvergentSet
        { _context = Context (response ^. Proto.context)
        , _key = key
        , _newValue = value
        , _oldValue = value
        }

-- | Put an eventually-convergent set.
putSet ::
     MonadIO m
  => Handle -- ^
  -> ConvergentSet ByteString -- ^
  -> m (Either PutSetError (ConvergentSet ByteString))
putSet handle set =
  liftIO (retrying 1000000 (putSet_ handle set))

putSet_ ::
     Handle
  -> ConvergentSet ByteString
  -> IO (Maybe (Either PutSetError (ConvergentSet ByteString)))
putSet_
    handle
    (ConvergentSet context key@(Key bucketType _ _) newValue oldValue) =

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
              & Proto.setOp .~ toProto newValue oldValue)
        & Proto.returnBody .~ True

-- TODO set update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    fromResponse ::
         Proto.DtUpdateResp
      -> ConvergentSet ByteString
    fromResponse response =
      ConvergentSet
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
        value :: HashSet ByteString
        value =
          HashSet.fromList (response ^. Proto.setValue)

toProto ::
     HashSet ByteString -- ^ New value
  -> HashSet ByteString -- ^ Old value
  -> Proto.SetOp -- ^ Delta
toProto newValue oldValue =
  Proto.defMessage
    & Proto.adds .~ adds
    & Proto.removes .~ removes

  where
    adds :: [ByteString]
    adds =
      HashSet.toList (HashSet.difference newValue oldValue)

    removes :: [ByteString]
    removes =
      HashSet.toList (HashSet.difference oldValue newValue)
