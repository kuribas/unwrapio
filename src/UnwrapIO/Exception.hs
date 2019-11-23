{-# LANGUAGE ExistentialQuantification #-}

module UnwrapIO.Exception where
import Control.Monad.IO.Unwrap
import qualified Control.Exception as E

catch :: (E.Exception e, UnwrapIO m) => m a -> (e -> m a) -> m a
catch block handler =
  rewrapIO $ E.catch <$> unwrapIO block <*> unwrapIO_CB handler

data WrappedHandler m a = forall e. E.Exception e => WrappedHandler (e -> m a)

catches :: (UnwrapIO m) => m a -> [WrappedHandler m a] -> m a 
catches m handlers =
  rewrapIO $ E.catches <$> unwrapIO m <*> traverse unwrapHandler handlers
  where
    unwrapHandler (WrappedHandler f) = E.Handler <$> unwrapIO_CB f

catchJust :: (UnwrapIO m, E.Exception e)
          => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust select block handler =
  rewrapIO $ E.catchJust select <$> unwrapIO block <*> unwrapIO_CB handler

handle :: (E.Exception e, UnwrapIO m) => (e -> m a) -> m a -> m a
handle = flip catch

handleJust :: (UnwrapIO m, E.Exception e)
           => (e -> Maybe b) -> (b -> m a) -> m a ->  m a
handleJust s = flip (catchJust s)

try :: (E.Exception e, UnwrapIO m) => m a -> m (Either e a)
try block =
  rewrapIO $ fmap (fmap (either (pure . Left) (fmap Right))) $
  E.try <$> unwrapIO block

tryJust :: (E.Exception e, UnwrapIO m)
        => (e -> Maybe b) -> m a -> m (Either b a)
tryJust test block =
  rewrapIO $ fmap (fmap (either (pure . Left) (fmap Right))) $
  E.tryJust test <$> unwrapIO block

bracket :: (UnwrapIO m) => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracket acquire release = unwrapCallback (E.bracket acquire release)

bracket_ :: (UnwrapIO m) => IO a -> IO b -> m c -> m c
bracket_ acquire release block =
  rewrapIO $ E.bracket_ acquire release <$> unwrapIO block

bracketOnError :: (UnwrapIO m) => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracketOnError acquire onError = unwrapCallback $
  E.bracketOnError acquire onError

finally :: (UnwrapIO m) => m a -> IO b -> m a
finally block final = rewrapIO $ flip E.finally final <$> (unwrapIO block)

onException :: (UnwrapIO m) => m a -> IO b -> m a
onException block final =
  rewrapIO $ flip E.onException final <$> (unwrapIO block)

