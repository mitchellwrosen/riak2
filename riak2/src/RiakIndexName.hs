module RiakIndexName
  ( IndexName(..)
  , unIndexName
  , makeIndexName
  , unsafeMakeIndexName
  , fromBucketProps
  ) where

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto
import qualified Data.Text       as Text


-- | An valid index name contains ASCII characters in the range @32-127@, less
-- the character '/'.
newtype IndexName
  = IndexName { _unIndexName :: Text }
  deriving stock (Eq)
  deriving newtype (Show)

unIndexName ::
     IndexName -- ^
  -> Text
unIndexName =
  _unIndexName

makeIndexName ::
     Text -- ^
  -> Maybe IndexName
makeIndexName name = do
  guard (Text.all valid name)
  pure (IndexName name)

  where
    valid :: Char -> Bool
    valid c =
      c >= '\32' && c <= '\127' && c /= '/'

unsafeMakeIndexName ::
     Text -- ^
  -> IndexName
unsafeMakeIndexName =
  IndexName

fromBucketProps :: Proto.RpbBucketProps -> Maybe IndexName
fromBucketProps props =
  IndexName . decodeUtf8 <$>
    (props ^. Proto.maybe'searchIndex)
