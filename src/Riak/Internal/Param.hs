{-# LANGUAGE DataKinds, FlexibleContexts, FunctionalDependencies,
             FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators #-}

module Riak.Internal.Param
  ( (:=)((:=))
  , Has
  , param
  ) where

import Data.Kind    (Type)
import Data.Proxy   (Proxy)
import GHC.TypeLits (Symbol)
import Data.Default.Class
import Data.Proxy

data (:=) (k :: Symbol) (v :: Type) =
  Proxy k := Maybe v

instance Default ( k0 := v0 ) where def = Proxy := Nothing
instance Default ( k0 := v0, k1 := v1 ) where def = (Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)
instance Default ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12 ) where def = (Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing, Proxy := Nothing)

class Has (k :: Symbol) v x | x k -> v where
  param :: Proxy k -> v -> x -> x

instance Has k0 v0 ( k0 := v0 ) where
  param p v _ = p := Just v

instance Has k0 v0 ( k0 := v0, k1 := v1 ) where
  param p v (_, x1) = (p := Just v, x1)


instance Has k1 v1 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8 ) where
  param p v (x0, _, x2, x3, x4, x5, x6, x7, x8) = (x0, p := Just v, x2, x3, x4, x5, x6, x7, x8)

instance Has k8 v8 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8 ) where
  param p v (x0, x1, x2, x3, x4, x5, x6, x7, _) = (x0, x1, x2, x3, x4, x5, x6, x7, p := Just v)
