-- TODO Move exchange/stream to Riak.Internal.Client, inline everything else

module Riak.Interface
  ( Interface
  , Interface.connect
  , Interface.disconnect
  -- , deleteIndex
  -- , getIndex
  -- , getSchema
  , mapReduce
  -- , putIndex
  -- , putSchema
  -- , search
  , Result(..)
  ) where

import Riak.Interface.Signature (Interface)
import Riak.Proto
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto.Lens          as L

import Control.Exception      (Exception, throwIO)
import Control.Foldl          (FoldM(..))
import Control.Lens           (view)


data Result a
  = Success a
  | Failure ErrorResponse
  | ConnectionClosed
  deriving stock (Functor, Show)

data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

exchange ::
     Interface
  -> Request
  -> (Response -> Maybe a)
  -> IO (Result a)
exchange conn request f =
  Interface.exchange conn request >>= \case
    Nothing ->
      pure ConnectionClosed

    Just (ResponseError response) ->
      pure (Failure response)

    Just response ->
      case f response of
        Nothing ->
          throwIO (UnexpectedResponse request response)

        Just response' ->
          pure (Success response')

stream ::
     forall a r.
     Interface
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Result r)
stream iface request f done (FoldM step initial extract) =
  Interface.stream iface request callback

  where
    callback :: IO (Maybe Response) -> IO (Result r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= \case
            Nothing ->
              pure ConnectionClosed

            Just (ResponseError response) ->
              pure (Failure response)

            Just response ->
              case f response of
                Nothing ->
                  throwIO (UnexpectedResponse request response)

                Just response' -> do
                  value' <-
                    step value response'

                  if done response'
                    then
                      Success <$> extract value'
                    else
                      loop value'


--------------------------------------------------------------------------------
-- Riak API
--------------------------------------------------------------------------------

-- deleteIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexDeleteReq -- ^
--   -> IO (Result RpbDelResp)
-- deleteIndex =
--   exchange

-- getIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexGetReq -- ^
--   -> IO (Result RpbYokozunaIndexGetResp)
-- getIndex =
--   exchange

-- getSchema
--   :: Interface -- ^
--   -> RpbYokozunaSchemaGetReq -- ^
--   -> IO (Result RpbYokozunaSchemaGetResp)
-- getSchema =
--   exchange

-- putIndex
--   :: Interface -- ^
--   -> RpbYokozunaIndexPutReq -- ^
--   -> IO (Result RpbEmptyPutResp)
-- putIndex =
--   exchange

-- putSchema
--   :: Interface -- ^
--   -> RpbYokozunaSchemaPutReq -- ^
--   -> IO (Result RpbEmptyPutResp)
-- putSchema =
--   exchange

mapReduce
  :: Interface -- ^
  -> MapReduceRequest -- ^
  -> FoldM IO MapReduceResponse r -- ^
  -> IO (Result r)
mapReduce conn request =
  stream
    conn
    (RequestMapReduce request)
    (\case
      ResponseMapReduce response -> Just response
      _ -> Nothing)
    (view L.done)

-- search
--   :: Interface -- ^
--   -> RpbSearchQueryReq -- ^
--   -> IO (Result RpbSearchQueryResp)
-- search =
--   exchange
