-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/search/>
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/searching-data-types/>
--
-- CRDT search examples:
--
-- * Counters
--
--     The default Solr schema indexes counters under the field name
--     @"counter"@.
--
--     For example, to search for counters in bucket @"foo"@ with values greater
--     than 100:
--
--     > _yz_rb:foo AND counter:{100 TO *]
--
-- * Sets
--
-- * Maps
--
--     The default Solr schema indexes dynamic fields ending in @"_counter"@,
--     @"_flag"@, @"_register"@, @"_set"@, and the default
--     @application/riak_map@ extractor dynamically renames map keys per their
--     type by suffixing them with @"_counter"@, @"_flag"@, @"_map"@,
--     @"_register"@, or @"_set@", using @'.'@ as a delimiter for nested maps.
--
--     For example, to search for maps with an inner map named @"foo"@ that
--     contain an inner map named @"bar"@ that contain a set named @"baz"@ that
--     contain the string @"qux"@:
--
--     > foo_map.bar_map.baz_set:qux

module Riak.Search
  ( search
  , SearchOpts(..)
  , SearchResults(..)
  ) where

import Libriak.Handle          (Handle)
import Riak.Internal.Error
import Riak.Internal.IndexName (IndexName(..))
import Riak.Internal.Prelude

import qualified Libriak.Handle           as Handle
import qualified Libriak.Proto            as Proto
import qualified Riak.Internal.Proto.Pair as Proto.Pair

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

  fromHandleResult
    (Left . parseSearchError index)
    fromResponse
    (Handle.search handle request)

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
