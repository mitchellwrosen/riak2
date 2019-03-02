module RiakSearch where

import Libriak.Response (Response(..))
import RiakError
import RiakHandle       (Handle)
import RiakIndexName    (IndexName(..))

import qualified Libriak.Proto as Proto
import qualified RiakHandle    as Handle
import qualified RiakProtoPair as Proto.Pair

import Control.Lens       ((.~), (^.))
import Data.Default.Class (Default(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


data SearchResults
  = SearchResults
  { documents :: ![[(ByteString, ByteString)]]
    -- ^ Search results
  , maxScore :: !Float
  , numFound :: !Word32
    -- ^ Number of search results (not all of which may have been returned,
    -- per @rows@).
  } deriving stock (Generic, Show)

data SearchOpts
  = SearchOpts
  { fieldList :: ![ByteString]
    -- ^ The fields to return (the empty list means all fields).
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
  -> IndexName -- ^ Search index
  -> ByteString -- ^ Search query
  -> SearchOpts -- ^ Search options
  -> m (Either SearchError SearchResults)
search
    handle
    index
    query
    SearchOpts { fieldList, filter, presort, rows, sort, start } = liftIO $

  Handle.search handle request >>= \case
    Left err ->
      pure (Left (HandleError err))

    Right (Left err) ->
      pure (Left (parseSearchError index err))

    Right (Right (RespRpbSearchQuery response)) ->
      pure (Right (fromResponse response))

  where
    request :: Proto.RpbSearchQueryReq
    request =
      Proto.defMessage
        & Proto.fl .~ fieldList
        & Proto.index .~ encodeUtf8 (unIndexName index)
        & Proto.maybe'filter .~ filter
        & Proto.maybe'presort .~ presort
        & Proto.maybe'rows .~ rows
        & Proto.maybe'sort .~ sort
        & Proto.maybe'start .~ start
        & Proto.q .~ query

    fromResponse :: Proto.RpbSearchQueryResp -> SearchResults
    fromResponse response =
      SearchResults
        { documents = map fromProtoSearchDoc (response ^. Proto.docs)
        , maxScore = response ^. Proto.maxScore
        , numFound = response ^. Proto.numFound
        }

    fromProtoSearchDoc :: Proto.RpbSearchDoc -> [(ByteString, ByteString)]
    fromProtoSearchDoc doc =
      map Proto.Pair.toTuple (doc ^. Proto.fields)

parseSearchError :: IndexName -> ByteString -> SearchError
parseSearchError index err
  | isIndexDoesNotExistError1 err =
      IndexDoesNotExistError index
  | isSearchFailedError err =
      SearchFailedError
  | isUnknownMessageCode err =
      SearchNotEnabledError
  | otherwise =
      UnknownError (decodeUtf8 err)
