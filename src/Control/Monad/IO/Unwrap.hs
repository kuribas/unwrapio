{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Monad.IO.Unwrap (
  withUnwrappedCallback, withUnwrappedMonad, 
  unwrapIO, rewrapIO, rewrapIO_traverse, unwrapIO_CB,
  UnwrapIO) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Writer

-- | A class which allows unwrapping of the target monad into a IO monad
-- returning a pure result.
class MonadIO m => UnwrapIO m where
  -- | Returns an unwrapper, using the current monadic context.  The
  -- unwrapper unwraps the target monad into an IO monad. Note that
  -- the inner monad in the result is pure, correct instances
  -- don't perform any IO actions in the inner monad.  You
  -- probably shouldn't be using this function, use the helpers
  -- instead, like `withUnwrappedMonad`.
  getUnwrapper :: m (m a -> IO (m a))

instance UnwrapIO IO where
  getUnwrapper = pure $ fmap pure
  {-# INLINE getUnwrapper #-}

instance UnwrapIO m => UnwrapIO (StateT s m) where
  getUnwrapper = do
    s <- get
    innerUnwrapper <- lift getUnwrapper
    pure $ \m -> fmap (StateT . const) $
                 innerUnwrapper $ 
                 runStateT m s
  {-# INLINE getUnwrapper #-}

instance UnwrapIO m => UnwrapIO (MaybeT m) where
  getUnwrapper = do
    innerUnwrapper <- lift getUnwrapper
    pure $ \m -> fmap MaybeT $ innerUnwrapper $
                 runMaybeT m
  {-# INLINE getUnwrapper #-}
  
instance UnwrapIO m => UnwrapIO (ExceptT e m) where
  getUnwrapper = do
    innerUnwrapper <- lift getUnwrapper
    pure $ \m -> fmap ExceptT $ innerUnwrapper $
                 runExceptT m
  {-# INLINE getUnwrapper #-}
 
instance UnwrapIO m => UnwrapIO (ReaderT r m) where
  getUnwrapper = do
    s <- ask
    innerUnwrapper <- lift getUnwrapper
    pure $ \m -> fmap (ReaderT . const) $ 
                 innerUnwrapper $ runReaderT m s
  {-# INLINE getUnwrapper #-}

instance (UnwrapIO m, Monoid w) => UnwrapIO (WriterT w m) where
  getUnwrapper = do
    innerUnwrapper <- lift getUnwrapper
    pure $ \m ->
      fmap WriterT $
      innerUnwrapper $ 
      runWriterT m
  {-# INLINE getUnwrapper #-}

instance UnwrapIO m => UnwrapIO (IdentityT m) where
  getUnwrapper = do
    innerUnwrapper <- lift getUnwrapper
    pure $ \m -> fmap IdentityT $ innerUnwrapper $ runIdentityT m
  {-# INLINE getUnwrapper #-}

-- | Unwrap a monad.
unwrapIO :: UnwrapIO m => m a -> m (IO (m a))
unwrapIO m = (\unwrapper -> unwrapper m) <$> getUnwrapper
{-# INLINE unwrapIO #-}
  
-- | Rewrap the monad.
rewrapIO :: MonadIO m => m (IO (m a)) -> m a
rewrapIO m = join (m >>= liftIO)
{-# INLINE rewrapIO #-}

-- | Unwrap a callback.
unwrapIO_CB :: (Monad m, UnwrapIO m) => (a -> m b) -> m (a -> IO (m b))
unwrapIO_CB f = (\unwrapper -> unwrapper . f) <$> getUnwrapper
{-# INLINE unwrapIO_CB #-}    

-- | More general version of rewrapIO, for when the callback returns the main
-- result in a traversable (for example `try`).
rewrapIO_traverse :: (MonadIO m, Traversable f) => m (IO (f (m a))) -> m (f a)
rewrapIO_traverse m = m >>= liftIO >>= sequence
{-# INLINE rewrapIO_traverse #-}

-- | Lift a resource handling function into the monad.  Unwraps the
-- callback, passes it to the given function, and rewraps the
-- result. The typical usecase is to handle safe acquiring and
-- releasing of resources.
withUnwrappedCallback :: UnwrapIO m => ((a -> IO (m b)) -> IO (m b))
             -> (a -> m b) -> m b
withUnwrappedCallback withCb cb = rewrapIO $ withCb <$> unwrapIO_CB cb
{-# INLINE withUnwrappedCallback #-}

-- | Lift a function of IO monads into a function of the target monad.
withUnwrappedMonad :: UnwrapIO m => (IO (m a) -> IO (m a)) -> m a -> m a
withUnwrappedMonad f m = rewrapIO $ f <$> unwrapIO m
{-# INLINE withUnwrappedMonad #-}
