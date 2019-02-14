module Riak.Internal.IndexName where

import Riak.Internal.Prelude

import qualified Text


-- | An valid index name contains ASCII characters in the range @32-127@, less
-- the character '/'.
newtype IndexName
  = IndexName { unIndexName :: Text }
  deriving stock (Show)

makeIndexName :: Text -> Maybe IndexName
makeIndexName name = do
  guard (Text.all valid name)
  pure (IndexName name)

  where
    valid :: Char -> Bool
    valid c =
      c >= '\32' && c <= '\127' && c /= '/'

unsafeMakeIndexName :: Text -> IndexName
unsafeMakeIndexName =
  IndexName
