module Riak.Interface
  ( UnexpectedResponse(..)
  , delete
  , get
  , getBucket
  , listKeys
  , put
  , resetBucket
  , secondaryIndex
  , setBucket
  ) where

import Riak.Interface.Signature (Interface)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Interface.Signature as Interface
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Control.Exception (Exception, throwIO)
import Control.Foldl     (FoldM(..))
import Control.Lens      (view, (^.))
import Data.ByteString   (ByteString)


data UnexpectedResponse
  = UnexpectedResponse !Request !Response
  deriving stock (Show)
  deriving anyclass (Exception)

delete ::
     Interface
  -> Proto.DeleteRequest
  -> IO (Either ByteString ())
delete iface request =
  exchange
    iface
    (RequestDelete request)
    (\case
      ResponseDelete{} -> Just ()
      _ -> Nothing)

get ::
     Interface
  -> Proto.GetRequest
  -> IO (Either ByteString Proto.GetResponse)
get iface request =
  exchange
    iface
    (RequestGet request)
    (\case
      ResponseGet response -> Just response
      _ -> Nothing)

getBucket ::
     Interface
  -> Proto.GetBucketPropertiesRequest
  -> IO (Either ByteString Proto.BucketProperties)
getBucket iface request =
  exchange
    iface
    (RequestGetBucketProperties request)
    (\case
      ResponseGetBucketProperties response -> Just (response ^. L.props)
      _ -> Nothing)

listKeys ::
     Interface
  -> Proto.ListKeysRequest
  -> FoldM IO Proto.ListKeysResponse r
  -> IO (Either ByteString r)
listKeys iface request =
  stream
    iface
    (RequestListKeys request)
    (\case
      ResponseListKeys response -> Just response
      _ -> Nothing)
    (view L.done)

put ::
     Interface
  -> Proto.PutRequest
  -> IO (Either ByteString Proto.PutResponse)
put iface request =
  exchange
    iface
    (RequestPut request)
    (\case
      ResponsePut response -> Just response
      _ -> Nothing)

resetBucket ::
     Interface
  -> Proto.ResetBucketPropertiesRequest
  -> IO (Either ByteString ())
resetBucket iface request =
  exchange
    iface
    (RequestResetBucketProperties request)
    (\case
      ResponseResetBucketProperties _ -> Just ()
      _ -> Nothing)

setBucket ::
     Interface
  -> Proto.SetBucketPropertiesRequest
  -> IO (Either ByteString ())
setBucket iface request =
  exchange
    iface
    (RequestSetBucketProperties request)
    (\case
      ResponseSetBucketProperties{} -> Just ()
      _ -> Nothing)

secondaryIndex ::
     Interface
  -> Proto.SecondaryIndexRequest
  -> FoldM IO Proto.SecondaryIndexResponse r
  -> IO (Either ByteString r)
secondaryIndex iface request =
  stream
    iface
    (RequestSecondaryIndex request)
    (\case
      ResponseSecondaryIndex response -> Just response
      _ -> Nothing)
    (view L.done)

exchange ::
     Interface
  -> Request
  -> (Response -> Maybe a)
  -> IO (Either ByteString a)
exchange iface request f =
  Interface.exchange iface request >>= \case
    ResponseError response ->
      pure (Left (response ^. L.errmsg))

    response ->
      case f response of
        Nothing ->
          throwIO (UnexpectedResponse request response)

        Just response' ->
          pure (Right response')

stream ::
     forall a r.
     Interface
  -> Request -- ^ Request
  -> (Response -> Maybe a) -- ^ Correct response?
  -> (a -> Bool) -- ^ Done?
  -> FoldM IO a r -- ^ Fold responses
  -> IO (Either ByteString r)
stream iface request f done (FoldM step initial extract) =
  Interface.stream iface request callback

  where
    callback :: IO Response -> IO (Either ByteString r)
    callback recv =
      loop =<< initial

      where
        loop value =
          recv >>= \case
            ResponseError response ->
              pure (Left (response ^. L.errmsg))

            response ->
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
  :: Interface
  -> Proto.GetCrdtRequest
  -> IO (Either ByteString Proto.GetCrdtResponse)
getCrdt iface request =
  exchange
    iface
    (RequestGetCrdt request)
    (\case
      ResponseGetCrdt response -> Just response
      _ -> Nothing)

updateCrdt
  :: Interface -- ^
  -> Proto.UpdateCrdtRequest -- ^
  -> IO (Either ByteString Proto.UpdateCrdtResponse)
updateCrdt iface request =
  exchange
    iface
    (RequestUpdateCrdt request)
    (\case
      ResponseUpdateCrdt response -> Just response
      _ -> Nothing)
