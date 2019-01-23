module Riak.Client
  ( -- * Client
    Client
  , new
    -- * Object operations
    -- ** Get object
  , get
  , getHead
  , getIfModified
  , getHeadIfModified
  , IfModified(..)
    -- ** Put object
  , put
  , putGet
  , putGetHead
    -- ** Delete object
  , delete
    -- * Counter operations
    -- ** Get counter
  , getCounter
    -- ** Update counter
  , updateCounter
  ) where

import Riak.Internal.Client
import Riak.Internal.Client.Counter
import Riak.Internal.Client.Object
