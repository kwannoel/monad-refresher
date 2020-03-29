# monad-refresher
*Best way to learn something is to do it.*

# Laws
**functors**

| Law         | definition                       |
|-------------|----------------------------------|
| identity    | `fmap id = id`                   |
| composition | `fmap (f . g) = fmap f . fmap g` |

**Applicatives**

| Law          | definition                                   |
|--------------|----------------------------------------------|
| identity     | pure id <*> v = v                            |
| composition  | pure (.) <*> u <*> v <*> w = u <*> (v <*> w) |
| homomorphism | pure f <*> pure x = pure (f x)               |
| interchange  | u <*> pure y = pure ($ y) <*> u              |

**Monads**

| Law            | definition                               |
|----------------|------------------------------------------|
| left identity  | return a >>= f == f a                    |
| right identity | m >>= return == m                        |
| associativity  | (m >>= f) >>= g = m >>= (\x -> fx >>= g) |

**Monad Transformers**

| Law           | definition                             |
|---------------|----------------------------------------|
| identity      | lift . return = return                 |
| associativity | lift (m >>= f) = lift m >>= (lift . f) |

References:

http://dev.stephendiehl.com/hask/#monad-transformers

https://haskellbook.com/
