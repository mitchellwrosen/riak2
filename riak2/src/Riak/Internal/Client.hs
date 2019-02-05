module Riak.Internal.Client
  ( module Riak.Internal.Client
  , Error(..)
  , UnexpectedResponse(..)
  ) where

import Riak.Interface.Signature (Interface)
import Riak.Internal.Prelude
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Control.Foldl (FoldM(..))


-- TODO rename Interface to Client
type Client
  = Interface

data Error
  = Error Text
  | RemoteShutdown
  deriving stock (Show)

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
  -> IO (Either Error a)
exchange client request f =
  Interface.exchange client request >>= \case
    Nothing ->
      pure (Left RemoteShutdown)

    Just (ResponseError response) ->
      pure (Left (Error (decodeUtf8 (response ^. L.errmsg))))

    Just response ->
      case f response of
        Nothing ->
          throwIO (UnexpectedResponse request response)

        Just response' ->
          pure (Right response')

stream ::
     forall a r.
     Client
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Either Error r)
stream client request f done (FoldM step initial extract) =
  Interface.stream client request callback

  where
    callback :: IO (Maybe Response) -> IO (Either Error r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= \case
            Nothing ->
              pure (Left RemoteShutdown)

            Just (ResponseError response) ->
              pure (Left (Error (decodeUtf8 (response ^. L.errmsg))))

            Just response ->
              case f response of
                Nothing ->
                  throwIO (UnexpectedResponse request response)

                Just response' -> do
                  value' <-
                    step value response'

                  if done response'
                    then
                      Right <$> extract value'
                    else
                      loop value'

getCrdt
  :: Client
  -> Proto.GetCrdtRequest
  -> IO (Either Error Proto.GetCrdtResponse)
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
  -> IO (Either Error Proto.UpdateCrdtResponse)
updateCrdt client request =
  exchange
    client
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)
