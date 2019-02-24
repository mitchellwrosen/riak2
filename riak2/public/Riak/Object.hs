module Riak.Object
  ( -- * Object operations
    get
  , put
  , delete
    -- ** Get variants
  , getHead
  , getIfModified
  , getHeadIfModified
    -- ** Put variants
  , putGet
  , putGetHead
    -- * Object
  , Object(..)
  , newObject
    -- * Options
  , GetOpts(..)
  , PutOpts(..)
  , DeleteOpts(..)
  ) where

import RiakDeleteOpts (DeleteOpts(..))
import RiakGetOpts    (GetOpts(..))
import RiakObject
import RiakPutOpts    (PutOpts(..))
