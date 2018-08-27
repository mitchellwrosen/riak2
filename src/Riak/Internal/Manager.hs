{-# LANGUAGE NoImplicitPrelude #-}

module Riak.Internal.Manager where

import Data.Primitive.UnliftedArray

import Riak.Internal.Connection
import Riak.Internal.Prelude

type ConnectionArray
  = UnliftedArray (TVar (Maybe RiakConnection))

data Manager
  = Manager !ConnectionArray

createManager :: IO Manager
createManager = do
  let n = 4 :: Int

  conns <- unsafeNewUnliftedArray n

  for_ [1..n] $ \i ->
    newTVarIO Nothing >>= writeUnliftedArray conns i

  conns' <- unsafeFreezeUnliftedArray conns

  pure (Manager conns')
