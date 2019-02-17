module Riak.Index
  ( -- * Index
    getIndex
  , getIndexes
  , putIndex
  , PutIndexOpts(..)
  , deleteIndex
  , Index(..)
    -- * Index name
  , IndexName
  , makeIndexName
  , unsafeMakeIndexName
  ) where

import RiakIndex
import RiakIndexName
