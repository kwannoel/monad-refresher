# monad-refresher

## Laws
**functors**

| Law         | definition                       |
|-------------|----------------------------------|
| identity    | `fmap id = id`                   |
| composition | `fmap (f . g) = fmap f . fmap g` |

**Applicatives**

| Law          | definition                                     |
|--------------|------------------------------------------------|
| identity     | `pure id <*> v = v`                            |
| composition  | `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)` |
| homomorphism | `pure f <*> pure x = pure (f x)`               |
| interchange  | `u <*> pure y = pure ($ y) <*> u`              |

**Monads**

| Law            | definition                                 |
|----------------|--------------------------------------------|
| left identity  | `return a >>= f = f a`                     |
| right identity | `m >>= return = m`                         |
| associativity  | `(m >>= f) >>= g = m >>= (\x -> fx >>= g)` |

**Monad Transformers**

| Law | definition                               |
|-----|------------------------------------------|
| #1  | `lift . return = return`                 |
| #2  | `lift (m >>= f) = lift m >>= (lift . f)` |

## Questions I had

**Why is readerT defined as `r -> m a` instead of `m (r -> a)`**
Transformers should wrap the structure `m` around the **value / final result** we can get.

## References:

http://dev.stephendiehl.com/hask/#monad-transformers

https://haskellbook.com/

https://stackoverflow.com/questions/23342184/difference-between-monad-and-applicative-in-haskell
