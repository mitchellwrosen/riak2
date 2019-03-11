-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <https://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>

module RiakHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getHyperLogLog
  , updateHyperLogLog
  ) where

import RiakCrdt
import RiakError
import RiakHandle (Handle)
import RiakKey    (Key(..), isGeneratedKey)
import RiakUtils  (retrying)

import qualified RiakHandle as Handle
import qualified RiakKey    as Key

import Control.Lens ((.~), (^.))

import qualified Data.Riak.Proto as Proto


-- | An eventually-convergent HyperLogLog, which provides an approximate
-- cardinality of a set.
--
-- HyperLogLogs must be stored in a bucket type with the __@datatype = hll@__
-- property.
--
-- The @hllPrecision@ bucket type property controls the number of precision bits
-- to use. Valid values are 4-16 (inclusive), and the default value is 14. The
-- precision may only be decreased, never increased.
--
-- /Note/: HyperLogLogs do not contain a causal context, so it is not necessary
-- to read a HyperLogLog before updating it.
data ConvergentHyperLogLog a
  = ConvergentHyperLogLog
  { key :: !Key -- ^
  , value :: !a -- ^
  } deriving stock (Functor, Generic, Show)

-- | Get an eventually-convergent HyperLogLog.
getHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either GetHyperLogLogError (Maybe (ConvergentHyperLogLog Word64)))
getHyperLogLog handle key =
  liftIO (retrying 1000000 (getHyperLogLog_ handle key))

getHyperLogLog_ ::
     Handle -- ^
  -> Key -- ^
  -> IO (Maybe (Either GetHyperLogLogError (Maybe (ConvergentHyperLogLog Word64))))
getHyperLogLog_ handle key@(Key bucketType _ _) =
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

        -- TODO get hll opts
        -- & Proto.maybe'basicQuorum .~ undefined
        -- & Proto.maybe'nVal .~ undefined
        -- & Proto.maybe'notfoundOk .~ undefined
        -- & Proto.maybe'pr .~ undefined
        -- & Proto.maybe'r .~ undefined
        -- & Proto.maybe'sloppyQuorum .~ undefined
        -- & Proto.maybe'timeout .~ undefined

    fromResponse ::
         Proto.DtFetchResp
      -> Maybe (ConvergentHyperLogLog Word64)
    fromResponse response = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value

      pure ConvergentHyperLogLog
        { key = key
        , value = crdt ^. Proto.hllValue
        }

-- | Update an eventually-convergent HyperLogLog.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> ConvergentHyperLogLog [ByteString] -- ^
  -> m (Either UpdateHyperLogLogError (ConvergentHyperLogLog Word64))
updateHyperLogLog handle hll =
  liftIO (retrying 1000000 (updateHyperLogLog_ handle hll))

updateHyperLogLog_ ::
     Handle
  -> ConvergentHyperLogLog [ByteString]
  -> IO (Maybe (Either UpdateHyperLogLogError (ConvergentHyperLogLog Word64)))
updateHyperLogLog_
    handle
    (ConvergentHyperLogLog key@(Key bucketType _ _) value) =

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
        & Proto.op .~
            (Proto.defMessage
              & Proto.hllOp .~
                  (Proto.defMessage
                    & Proto.adds .~ value))
        & Proto.returnBody .~ True

-- TODO map update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    fromResponse :: Proto.DtUpdateResp -> ConvergentHyperLogLog Word64
    fromResponse response =
      ConvergentHyperLogLog
        { key =
            if isGeneratedKey key
              then
                case key of
                  Key bucketType bucket _ ->
                    Key bucketType bucket (response ^. Proto.key)
              else
                key
        , value =
            response ^. Proto.hllValue
        }
