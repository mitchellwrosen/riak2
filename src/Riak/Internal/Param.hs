{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             FunctionalDependencies, InstanceSigs, KindSignatures,
             MultiParamTypeClasses, TypeOperators #-}

module Riak.Internal.Param
  ( Default(..)
  , (:=)((:=))
  , Has
  , param
  ) where

import Data.Kind    (Type)
import Data.Proxy   (Proxy)
import Data.Proxy
import GHC.TypeLits (Symbol)

import qualified Data.Default.Class

import Proto.Riak

-- Temporarily use our own default class
-- Waiting on https://github.com/mauke/data-default/pull/19
class Default a where
  def :: a

instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where def = (def, def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g, Default h) => Default (a, b, c, d, e, f, g, h) where def = (def, def, def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g, Default h, Default i) => Default (a, b, c, d, e, f, g, h, i) where def = (def, def, def, def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g, Default h, Default i, Default j, Default k, Default l) => Default (a, b, c, d, e, f, g, h, i, j, k, l) where def = (def, def, def, def, def, def, def, def, def, def, def, def)

instance Default RpbContent where
  def = Data.Default.Class.def


data (:=) (k :: Symbol) (v :: Type) =
  Proxy k := Maybe v

instance Default (k := v) where
  def = Proxy := Nothing


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

instance Has k0 v0 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (_, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = (p := Just v, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
instance Has k1 v1 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, _, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = (x0, p := Just v, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
instance Has k2 v2 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, _, x3, x4, x5, x6, x7, x8, x9, x10, x11) = (x0, x1, p := Just v, x3, x4, x5, x6, x7, x8, x9, x10, x11)
instance Has k3 v3 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, _, x4, x5, x6, x7, x8, x9, x10, x11) = (x0, x1, x2, p := Just v, x4, x5, x6, x7, x8, x9, x10, x11)
instance Has k4 v4 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, _, x5, x6, x7, x8, x9, x10, x11) = (x0, x1, x2, x3, p := Just v, x5, x6, x7, x8, x9, x10, x11)
instance Has k5 v5 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, _, x6, x7, x8, x9, x10, x11) = (x0, x1, x2, x3, x4, p := Just v, x6, x7, x8, x9, x10, x11)
instance Has k6 v6 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, _, x7, x8, x9, x10, x11) = (x0, x1, x2, x3, x4, x5, p := Just v, x7, x8, x9, x10, x11)
instance Has k7 v7 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, x6, _, x8, x9, x10, x11) = (x0, x1, x2, x3, x4, x5, x6, p := Just v, x8, x9, x10, x11)
instance Has k8 v8 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, x6, x7, _, x9, x10, x11) = (x0, x1, x2, x3, x4, x5, x6, x7, p := Just v, x9, x10, x11)
instance Has k9 v9 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, x6, x7, x8, _, x10, x11) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, p := Just v, x10, x11)
instance Has k10 v10 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, _, x11) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, p := Just v, x11)
instance Has k11 v11 ( k0 := v0, k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11 ) where param p v (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, _) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, p := Just v)
