module Platform.Util.ErrorHandling where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither, liftMaybe)
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)

liftEither' :: ∀ e e' a m. MonadError e' m => (e -> e') -> Either e a -> m a
liftEither' f = liftEither <<< lmap f

liftSuccessMaybe
  :: ∀ e a m t
   . Monad m
  => MonadTrans t
  => MonadError e (t m)
  => e
  -> m (Maybe a)
  -> t m a
liftSuccessMaybe e x = lift x >>= liftMaybe e

liftSuccess
  :: ∀ e a m t
   . Monad m
  => MonadTrans t
  => MonadError e (t m)
  => m (Either e a)
  -> t m a
liftSuccess x = lift x >>= liftEither

liftSuccess'
  :: ∀ e e' a m t
   . Monad m
  => MonadTrans t
  => MonadError e' (t m)
  => (e -> e')
  -> m (Either e a)
  -> t m a
liftSuccess' f m = liftSuccess (lmap f <$> m)