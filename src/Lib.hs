module Lib where

data Writer r a = Writer (r, a)

instance Functor (Writer r) where
    fmap f (Writer (r, a)) = Writer (r, f a)

instance Monoid r => Applicative (Writer r) where
    pure a = Writer (mempty, a)
    Writer (r1, f) <*> Writer (r2, a) = Writer (r1 <> r2, f a)

instance Monoid r => Monad (Writer r) where
    Writer (r, a) >>= f = let Writer (r2, a2) = f a
                          in  Writer (r <> r2, a2)

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    fmap f r = Reader $ f <$> (runReader r)

instance Applicative (Reader r) where
    pure a = Reader (const a)
    Reader f <*> Reader a = Reader $ \r -> let fi = f r
                                               ai = a r
                                           in  fi ai

instance Monad (Reader r) where
    Reader ra >>= af = Reader $ \r -> let a = ra r
                                          Reader rb = af a
                                          b = rb r
                                      in  b

data State s a = State (s -> (a, s))

instance Functor (State s) where
    fmap f (State st) = State $ \s1 -> let (a, s2) = st s1
                                       in  (f a, s2)

instance Applicative (State s) where
    pure a = State (\x -> (a, x))
    State sf <*> State st = State $ \s -> let (f, s1) = sf s
                                              (a, s2) = st s1
                                          in  (f a, s2)

instance Monad (State s) where
    State m >>= f = State $ \s -> let (a1, s1) = m s
                                      State m2 = f a1
                                      (a2, s2) = m2 s1
                                  in (a2, s2)

-- Use monad transformers to compose monads
class MonadTrans t where
    lift :: Monad m => m a -> t m a

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

data WriterT r m a = WriterT { runWriterT :: m (r, a) }

instance Functor m => Functor (WriterT r m) where
    fmap f (WriterT mra) = WriterT $ fmap (\(r, a) -> (r, f a)) mra

instance (Applicative m, Monoid r) => Applicative (WriterT r m) where
    pure a = WriterT $ pure (mempty, a)
    -- Originally theres 2 layers of structure
    -- s.t. its structure looks like m (r, f)
    -- in order for the applicative operator to work
    -- it has to follow to type signature of (<*>)
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- so we have to transform m (r, f) into the form m (a -> b)
    WriterT mf <*> WriterT ma = WriterT $ fmap f mf <*> ma
      where f (r, f) = \(r2, a) -> (r <> r2, f a)

instance (Monad m, Monoid r) => Monad (WriterT r m) where
    WriterT ma >>= f = WriterT $ ma >>= f'
      -- a -> m (r, b)
      -- into
      -- (r, a) -> m (r, b)
      where f' = \(r, a) -> let g = \(r2, a) -> (r <> r2, a)
                                mb' = runWriterT $ f a
                            in fmap g mb'

instance Monoid r => MonadTrans (WriterT r) where
    lift = WriterT . fmap return

instance (Monoid r, MonadIO m) => MonadIO (WriterT r m) where
    liftIO = lift . liftIO

data ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f rma = ReaderT $ \r -> f <$> runReaderT rma r

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT $ \_ -> pure a
    mf <*> ma = ReaderT $ \r -> let mf' = runReaderT mf r
                                    ma' = runReaderT ma r
                                in  mf' <*> ma'

instance Monad m => Monad (ReaderT r m) where
    -- ma :: r -> m a
    -- f  :: a -> r -> m b
    ma >>= f = ReaderT $ \r -> let ma' = runReaderT ma r
                                   f'  = fmap (\re -> runReaderT re r) f
                               in  ma' >>= f'

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT r m) where
    fmap f = StateT . (fmap . fmap) (\(a, s) -> (f a, s)) . runStateT

instance Monad m => Applicative (StateT r m) where
    pure a = StateT $ \s -> pure (a, s)
    -- sf :: s -> m (f, s)
    -- sa :: s -> m (a, s)
    sf <*> sa = StateT $ \s -> let f = runStateT sf
                                   a = runStateT sa

                                   mfs = f s
                                   fsmas = \(f1, s1) -> let mas = a s1
                                                        in  fmap (\(a1, s') -> (f1 a1, s')) mas

                               in  mfs >>= fsmas

instance Monad m => Monad (StateT r m) where
    -- st :: s -> m (a, s)
    -- f :: a -> s -> m (b, s)
    st >>= f = StateT $ \s -> let mas = runStateT st s
                                  f' = \(a1, s1) -> runStateT (f a1) s1
                              in  mas >>= f'

instance MonadTrans (StateT r) where
    lift m = StateT $ \s -> fmap (\a -> (a, s)) m

instance MonadIO m => MonadIO (StateT r m) where
    liftIO = lift . liftIO
