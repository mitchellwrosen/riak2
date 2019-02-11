module Riak.Search
  ( search
  , bucketTypeField
  , bucketField
  , keyField
  , Search(..)
  , SearchOpts(..)
  ) where

import Libriak.Handle        (Handle)
import Riak.Internal.Prelude

import qualified Libriak.Handle           as Handle
import qualified Libriak.Proto            as Proto
import qualified Libriak.Proto.Lens       as L
import qualified Riak.Internal.Proto.Pair as Proto.Pair

import Control.Lens       ((.~), (^.))
import Data.Default.Class (Default(..))
import Data.Text.Encoding (encodeUtf8)


data Search
  = Search
  { index :: !Text
  , query :: !ByteString
  } deriving stock (Generic, Show)

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
  -> Search -- ^
  -> SearchOpts -- ^
  -> m (Either Handle.Error [[(ByteString, ByteString)]])
search
    handle
    Search { index, query }
    SearchOpts { fieldList, filter, presort, rows, sort, start } = liftIO $

  (fmap.fmap)
    fromResponse
    (Handle.search handle request)

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
