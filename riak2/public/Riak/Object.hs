module Riak.Object
  ( -- * Object operations
    get
  , put
  , delete
    -- ** Get variants
  , getWith
  , getHead
  , getHeadWith
  , getIfModified
  , getIfModifiedWith
  , getHeadIfModified
  , getHeadIfModifiedWith
    -- ** Put variants
  , putWith
  , putGet
  , putGetWith
  , putGetHead
  , putGetHeadWith
    -- ** Delete variants
  , deleteWith
    -- * Object
  , Object(..)
  , newObject
    -- * Options
  , GetOpts(..)
  , PutOpts(..)
  ) where

import RiakGetOpts (GetOpts(..))
import RiakObject
import RiakPutOpts (PutOpts(..))
