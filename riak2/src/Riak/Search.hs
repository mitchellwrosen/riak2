module Riak.Search
  ( search
  , bucketTypeField
  , bucketField
  , keyField
  , SearchOpts(..)
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Error
import Riak.Internal.Prelude

import qualified Libriak.Handle           as Handle
import qualified Libriak.Proto            as Proto
import qualified Libriak.Proto.Lens       as L
import qualified Riak.Internal.Proto.Pair as Proto.Pair

import Control.Lens       ((.~), (^.))
import Data.Default.Class (Default(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


data SearchOpts
  = SearchOpts
  { fieldList :: ![ByteString]
  , filter :: !(Maybe ByteString)
  , presort :: !(Maybe ByteString)
  , rows :: !(Maybe Word32)
  , sort :: !(Maybe ByteString)
  , start :: !(Maybe Word32)
  } deriving stock (Generic, Show)

instance Default SearchOpts where
  def :: SearchOpts
  def =
    SearchOpts
      { fieldList = []
      , filter = Nothing
      , presort = Nothing
      , rows = Nothing
      , sort = Nothing
      , start = Nothing
      }

search ::
     MonadIO m
  => Handle -- ^
  -> Text -- ^ Search index
  -> ByteString -- ^ Search query
  -> SearchOpts -- ^
  -> m (Either (Error 'SearchOp) [[(ByteString, ByteString)]])
search
    handle
    index
    query
    SearchOpts { fieldList, filter, presort, rows, sort, start } = liftIO $

  bimap parseSearchError fromResponse <$>
    Handle.search handle request

  where
    request :: Proto.SearchRequest
    request =
      Proto.defMessage
        & L.fieldList .~ fieldList
        & L.index .~ encodeUtf8 index
        & L.maybe'filter .~ filter
        & L.maybe'presort .~ presort
        & L.maybe'rows .~ rows
        & L.maybe'sort .~ sort
        & L.maybe'start .~ start
        & L.query .~ query

    parseSearchError :: Handle.Error -> Error 'SearchOp
    parseSearchError = \case
      Handle.ErrorHandle err ->
        HandleError err

      Handle.ErrorRiak err
        | isIndexDoesNotExistError err ->
            IndexDoesNotExistError index
        | isSearchFailedError err ->
            SearchFailedError
        | isSearchNotEnabledError err ->
            SearchNotEnabledError
        | otherwise ->
            UnknownError (decodeUtf8 err)

    fromResponse :: Proto.SearchResponse -> [[(ByteString, ByteString)]]
    fromResponse response =
      map fromDocument (response ^. L.docs)

    fromDocument :: Proto.Document -> [(ByteString, ByteString)]
    fromDocument doc =
      map Proto.Pair.toTuple (doc ^. L.fields)

bucketTypeField :: ByteString
bucketTypeField =
  "_yz_rt"

bucketField :: ByteString
bucketField =
  "_yz_rb"

keyField :: ByteString
keyField =
  "_yz_rk"
