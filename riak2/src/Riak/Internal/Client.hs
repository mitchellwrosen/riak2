module Riak.Internal.Client
  ( module Riak.Internal.Client
  -- TODO move Result(..) here
  , Result(..)
  ) where

import Riak.Interface.Signature (Interface)
import Riak.Internal.Prelude
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto               as Proto

import Control.Foldl (FoldM(..))


-- TODO rename Interface to Client
type Client
  = Interface

data Result a
  = Success a
  | Failure Proto.ErrorResponse
  | ConnectionClosed
  deriving stock (Functor, Show)

data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

new :: Interface -> Client
new =
  id

exchange ::
     Client
  -> Request
  -> (Response -> Maybe a)
  -> IO (Result a)
exchange client request f =
  Interface.exchange client request >>= \case
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
     Client
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Result r)
stream client request f done (FoldM step initial extract) =
  Interface.stream client request callback

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

getCrdt
  :: Client
  -> Proto.GetCrdtRequest
  -> IO (Result Proto.GetCrdtResponse)
getCrdt client request =
  exchange
    client
    (RequestGetCrdt request)
    (\case
      ResponseGetCrdt response -> Just response
      _ -> Nothing)

updateCrdt
  :: Client -- ^
  -> Proto.UpdateCrdtRequest -- ^
  -> IO (Result Proto.UpdateCrdtResponse)
updateCrdt client request =
  exchange
    client
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)
