{- |

Laws of functors:

1. identity

>>> fmap id = id

2. composition

>>> fmap (f . g) = fmap f . fmap g

Laws of Applicatives:

1. identity

>>> pure id <*> v = v

2. composition

>>> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

3. homomorphism

>>> pure f <*> pure x = pure (f x)

4. interchange

>>> u <*> pure y = pure ($ y) <*>

Laws of monads:

1. left identity:

>>> return a >>= f == f a

2. right identity

>>> m >>= return == m

3. associativity:

>>> (m >>= f) >>= g = m >>= (\x -> fx >>= g)

-}

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

data WriterT
