module Riak.Internal.Client where

import Riak.Interface (Interface)

newtype Client
  = Client
  { iface :: Interface
  }

new :: Interface -> Client
new =
  Client
