# monad-refresher
Laws of functors:

1. identity
> fmap id = id

2. composition 
> fmap (f . g) = fmap f . fmap g

Laws of Applicatives:

1. identity
> pure id <*> v = v

2. composition
> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

3. homomorphism
pure f <*> pure x = pure (f x)

4. interchange
u <*> pure y = pure ($ y) <*>

Laws of monads:

1. left identity: 
> return a >>= f == f a

2. right identity
> m >>= return == m

3. associativity:
> (m >>= f) >>= g = m >>= (\x -> fx >>= g)
