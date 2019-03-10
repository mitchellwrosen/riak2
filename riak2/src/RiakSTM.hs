-- | Misc. STM helpers

module RiakSTM
  ( TransactionalIO
  , transactionally
  , liftSTM

  , TCounter
  , newTCounter
  , readTCounter
  , incrTCounter
  , decrTCounter

  , registerOneShotEvent
  ) where

import Control.Concurrent.STM
import Control.Monad          (ap)


newtype TransactionalIO a
  = TransactionalIO { unTransactionalIO :: STM (Either a (IO a)) }

instance Applicative TransactionalIO where
  pure x =
    TransactionalIO (pure (Left x))

  (<*>) =
    ap

instance Alternative TransactionalIO where
  empty =
    TransactionalIO empty

  TransactionalIO x <|> TransactionalIO y =
    TransactionalIO (x <|> y)

instance Functor TransactionalIO where
  fmap f (TransactionalIO action) =
    TransactionalIO (bimap f (fmap f) <$> action)

instance Monad TransactionalIO where
  TransactionalIO mx >>= f =
    TransactionalIO $
      mx >>= \case
        Left x ->
          unTransactionalIO (f x)

        Right my ->
          pure (Right (my >>= transactionally . f))

instance MonadIO TransactionalIO where
  liftIO action =
    TransactionalIO (pure (Right action))

transactionally :: TransactionalIO a -> IO a
transactionally (TransactionalIO action) =
  atomically action >>= either pure id

liftSTM :: STM a -> TransactionalIO a
liftSTM action =
  TransactionalIO (Left <$> action)


newtype TCounter
  = TCounter (TVar Int)

newTCounter :: IO TCounter
newTCounter =
  TCounter <$> newTVarIO 0

readTCounter :: TCounter -> STM Int
readTCounter (TCounter var) =
  readTVar var

incrTCounter :: TCounter -> STM ()
incrTCounter (TCounter var) =
  modifyTVar' var (+1)

decrTCounter :: TCounter -> STM ()
decrTCounter (TCounter var) =
  modifyTVar' var (subtract 1)

registerOneShotEvent :: Int -> IO (STM ())
registerOneShotEvent micros = do
  var :: TVar Bool <-
    registerDelay micros

  pure
    (readTVar var >>= \case
      False -> retry
      True -> pure ())
