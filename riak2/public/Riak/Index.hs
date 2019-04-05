module Riak.Index
  ( -- * Index
    Index(..)
  , getIndex
  , getIndexes
  , putIndex
  , putIndexWith
  , PutIndexOpts(..)
  , deleteIndex
    -- * Index name
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  , unIndexName
  ) where

import RiakIndex
import RiakIndexName
