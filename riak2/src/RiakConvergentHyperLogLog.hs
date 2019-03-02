-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/data-types/hyperloglogs/>
-- * <https://basho.com/posts/technical/what-in-the-hell-is-hyperloglog/>
-- * <https://github.com/basho/riak_kv/blob/develop/docs/hll/hll.pdf>

module RiakConvergentHyperLogLog
  ( ConvergentHyperLogLog(..)
  , getConvergentHyperLogLog
  , updateConvergentHyperLogLog
  ) where

import Libriak.Response (Response(..))
import RiakCrdt
import RiakError
import RiakHandle       (Handle)
import RiakKey          (Key(..), isGeneratedKey)
import RiakUtils        (retrying)

import qualified Libriak.Proto as Proto
import qualified RiakHandle    as Handle
import qualified RiakKey       as Key

import Control.Lens ((.~), (^.))


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

-- | Get a HyperLogLog.
getConvergentHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> Key -- ^
  -> m (Either GetConvergentHyperLogLogError (Maybe (ConvergentHyperLogLog Word64)))
getConvergentHyperLogLog handle key =
  liftIO (retrying 1000000 (getConvergentHyperLogLog_ handle key))

getConvergentHyperLogLog_ ::
     Handle -- ^
  -> Key -- ^
  -> IO (Maybe (Either GetConvergentHyperLogLogError (Maybe (ConvergentHyperLogLog Word64))))
getConvergentHyperLogLog_ handle key@(Key bucketType _ _) =
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
         Response 81
      -> Maybe (ConvergentHyperLogLog Word64)
    fromResponse (RespDtFetch response) = do
      crdt :: Proto.DtValue <-
        response ^. Proto.maybe'value

      pure ConvergentHyperLogLog
        { key = key
        , value = crdt ^. Proto.hllValue
        }

-- | Update a HyperLogLog.
--
-- /See also/: 'Riak.Context.newContext', 'Riak.Key.generatedKey'
updateConvergentHyperLogLog ::
     MonadIO m
  => Handle -- ^
  -> ConvergentHyperLogLog [ByteString] -- ^
  -> m (Either UpdateConvergentHyperLogLogError (ConvergentHyperLogLog Word64))
updateConvergentHyperLogLog handle hll =
  liftIO (retrying 1000000 (updateConvergentHyperLogLog_ handle hll))

updateConvergentHyperLogLog_ ::
     Handle
  -> ConvergentHyperLogLog [ByteString]
  -> IO (Maybe (Either UpdateConvergentHyperLogLogError (ConvergentHyperLogLog Word64)))
updateConvergentHyperLogLog_
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

    fromResponse :: Response 83 -> ConvergentHyperLogLog Word64
    fromResponse (RespDtUpdate response) =
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
