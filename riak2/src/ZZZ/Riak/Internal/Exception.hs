module ZZZ.Riak.Internal.Exception where

import Data.Typeable (cast)

import Riak.Internal.Prelude
import ZZZ.Riak.Internal.Types


-- data RiakException where
--   RiakException :: Exception e => e -> RiakException
--   deriving anyclass (Exception)

-- instance Show RiakException where
--   show (RiakException e) =
--     show e

-- riakExceptionToException :: Exception e => e -> SomeException
-- riakExceptionToException =
--   toException . RiakException

-- riakExceptionFromException :: Exception e => SomeException -> Maybe e
-- riakExceptionFromException e = do
--   RiakException e' <- fromException e
--   cast e'

-- -- TODO make RiakMapParseError a sub-exception of RiakParseError
-- data RiakObjectParseError
--   = RiakObjectParseError !RiakKey !Text
--   deriving stock (Show)
--   -- deriving Exception via ARiakException

-- instance Exception RiakObjectParseError where
--   fromException = riakExceptionFromException
--   toException = riakExceptionToException
