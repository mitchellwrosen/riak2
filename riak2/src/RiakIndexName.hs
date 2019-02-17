module RiakIndexName where

import qualified Data.Text as Text


-- | An valid index name contains ASCII characters in the range @32-127@, less
-- the character '/'.
newtype IndexName
  = IndexName { unIndexName :: Text }
  deriving stock (Eq)
  deriving newtype (Show)

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
