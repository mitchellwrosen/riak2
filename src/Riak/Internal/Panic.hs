{-# LANGUAGE DeriveAnyClass, DerivingStrategies, ExistentialQuantification,
             FlexibleInstances, InstanceSigs, TypeFamilies #-}

module Riak.Internal.Panic
  ( Panic
  , panic
  ) where

import Control.Monad.IO.Unlift
import Data.Text               (Text)
import UnliftIO.Exception      (Exception, throwIO)

import qualified Data.Text as Text

data Panic
  = forall a. Panicked a => Panic !Text a
  deriving anyclass (Exception)

instance Show Panic where
  show :: Panic -> String
  show (Panic s x) =
    Text.unpack s ++ ": " ++ panicked x

class Panicked a where
  panicked :: a -> String

instance {-# OVERLAPPABLE #-}
    ( a1 ~ Text
    , Show b1
    ) => Panicked (a1, b1) where
  panicked :: (Text, b1) -> String
  panicked =
    show

instance {-# OVERLAPPABLE #-}
    ( a1 ~ Text
    , a2 ~ Text
    , Show b1
    , Show b2
    )
    => Panicked ((a1, b1), (a2, b2)) where
  panicked :: ((Text, b1), (Text, b2)) -> String
  panicked =
    show

panic :: (MonadIO m, Panicked a) => Text -> a -> m void
panic s x =
  throwIO (Panic s x)

