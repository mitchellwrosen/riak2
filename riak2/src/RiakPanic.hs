module RiakPanic
  ( Panic
  , panic
  , impurePanic
  ) where

import Control.Exception (throw, throwIO)

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

-- Instances I happened to need...
instance {-# OVERLAPPABLE #-} (a1 ~ Text, Show b1)                                                             => Panicked  (a1, b1)                                where panicked = show
instance {-# OVERLAPPABLE #-} (a1 ~ Text, Show b1, a2 ~ Text, Show b2)                                         => Panicked ((a1, b1), (a2, b2))                     where panicked = show
-- instance {-# OVERLAPPABLE #-} (a1 ~ Text, Show b1, a2 ~ Text, Show b2, a3 ~ Text, Show b3, a4 ~ Text, Show b4) => Panicked ((a1, b1), (a2, b2), (a3, b3), (a4, b4)) where panicked = show

panic :: (MonadIO m, Panicked a) => Text -> a -> m void
panic s x =
  liftIO (throwIO (Panic s x))

impurePanic :: Panicked a => Text -> a -> void
impurePanic s x =
  throw (Panic s x)
