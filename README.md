# monad-refresher
*Best way to learn something is to do it.*

# Laws
Laws of functors:

1. identity

``` haskell
fmap id = id
```


2. composition

``` haskell
fmap (f . g) = fmap f . fmap g
```


Laws of Applicatives:

1. identity

``` haskell
pure id <*> v = v
```

2. composition

``` haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

3. homomorphism

``` haskell
pure f <*> pure x = pure (f x)
```

4. interchange

``` haskell
u <*> pure y = pure ($ y) <*>
```

Laws of monads:

1. left identity:

``` haskell
return a >>= f == f a
```

2. right identity

``` haskell
m >>= return == m
```

3. associativity:

``` haskell
(m >>= f) >>= g = m >>= (\x -> fx >>= g)
```

References:
http://dev.stephendiehl.com/hask/#monad-transformers
https://haskellbook.com/
