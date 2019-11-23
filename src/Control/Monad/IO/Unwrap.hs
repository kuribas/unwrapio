{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.IO.Unwrap where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.RWS as RWS

class MonadIO m => UnwrapIO m where
  unwrapIO_CB :: (a -> m b) -> m (a -> IO (m b))

instance UnwrapIO IO where
  unwrapIO_CB = pure . (fmap pure)

instance UnwrapIO m => UnwrapIO (StateT s m) where
  unwrapIO_CB f = do
    s <- get
    lift $ fmap (fmap (fmap (StateT . const))) $
      unwrapIO_CB $ \a -> runStateT (f a) s
    
instance UnwrapIO m => UnwrapIO (MaybeT m) where
  unwrapIO_CB f = lift $ fmap (fmap (fmap MaybeT)) $
                  unwrapIO_CB $ \a -> runMaybeT (f a)
  

instance UnwrapIO m => UnwrapIO (ExceptT e m) where
  unwrapIO_CB f = lift $ fmap (fmap (fmap ExceptT)) $
                  unwrapIO_CB $ \a -> runExceptT (f a)
  
instance UnwrapIO m => UnwrapIO (ReaderT r m) where
  unwrapIO_CB f = do
    r <- ask
    lift $ fmap (fmap (fmap lift)) $
      unwrapIO_CB $ \a -> runReaderT (f a) r

instance (UnwrapIO m, Monoid w) => UnwrapIO (WriterT w m) where
  unwrapIO_CB f = lift $ fmap (fmap (fmap WriterT)) $
                  unwrapIO_CB $ \a -> runWriterT (f a)
    
instance UnwrapIO m => UnwrapIO (IdentityT m) where
  unwrapIO_CB f = lift $ fmap (fmap (fmap IdentityT)) $
                  unwrapIO_CB $ runIdentityT . f

instance (UnwrapIO m, Monoid w) => UnwrapIO (RWS.RWST r w s m) where
  unwrapIO_CB f = do
    r <- RWS.ask
    s <- RWS.get
    lift $ fmap (fmap (fmap (\m -> RWS.RWST $ \_ _ -> m))) $ 
      unwrapIO_CB $ \a -> RWS.runRWST (f a) r s

-- | rewrap the IO monad into the monad.
rewrapIO :: MonadIO m => m (IO (m a)) -> m a
rewrapIO m = join (m >>= liftIO)

-- | unwrap the monad into a IO monad.
unwrapIO :: UnwrapIO m => m a -> m (IO (m a))
unwrapIO m = ($ ()) <$> unwrapIO_CB (const m)

-- | Call a function with an callback in the IO, with an UnwrapIO monad
-- callback.  It does the wrapping and unwrapping for you.  Typical
-- use is for functions which acquire resources, and ensure closing of
-- the resource.
unwrapCallback :: UnwrapIO m => ((a -> IO (m b)) -> IO (m b))
             -> (a -> m b) -> m b
unwrapCallback withCb cb = rewrapIO $ fmap withCb $ unwrapIO_CB cb
