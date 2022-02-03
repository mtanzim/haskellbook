# haskellbook

Exercises and content for the [haskellbook](https://haskellbook.com/)

Advent of code 2021 was moved [here](https://github.com/mtanzim/advent-of-code-2021)

## Why?

- Curiosity around functional programming

## Notes on critical concepts

### Monoid and Semigroup

[Chapter exercises](./ch15)

- A monoid is a **binary associative** operation with an **identity**

```haskell
mappend [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]
mappend [1..5] []
-- [1..5]
mappend [] [1..5] = [1..5]
-- [1..5]

-- Or more generally
mappend x mempty == x
mappend mempty x == x
```

- In Haskell, we define a type class for monoid as follows:

```haskell
class Semigroup m => Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

- Taking a concrete example for lists in Haskell:

```haskell
instance Semigroup [a] where
  (<>) = (++)
instance Monoid [a] where
  mempty = []
```

- Monoid laws can be highlighted as follows:

```haskell
-- left identity
mappend mempty x == x
-- right identity
mappend x mempty == x
-- associativity
mappend x (mappend y z) == mappend (mappend x y) z
mconcat == foldr mappend mempty
```

- A **Semigroup** is only a **binary associative** operation _without_ an indentity.

```haskell
class Semigroup a where
(<>) :: a -> a -> a
```

### Functor

[Chapter exercises](./ch16)

- A functor is a way of a applying a function inside of a structure, without altering the structure
- Seeing this with a type class:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

- Taking a concrete example with lists

```ghci
Prelude> map (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
```

- Note that for a type to have a `Functor` instance, it must be of kind `* -> *`
- For example, the following will **NOT** work:

```haskell
data FixMePls = FixMe | Pls deriving (Eq, Show)
instance Functor FixMePls where
  fmap = error "it doesn't matter, it won't compile"
```

- The reason is that `FixMePls` is of kind `*`
- Lists however, are of kind `* -> *`, because `[] -> a -> [a]` or in English: you need to provide the type of the member of the list to get the list type (ie: `Int`)
- With that in mind, the following will work:

```haskell
data FixMePls a = FixMe | Pls a deriving (Eq, Show)
instance Functor (FixMePls a) where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)
```

- You can stack functors, ie:

```ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> n = Nothing
Prelude> w = Just "woo"
Prelude> ave = Just "ave"
Prelude> lms = [n,w,ave]
Prelude> replaceWithP = const 'p'
Prelude> fmap replaceWithP lms
"ppp"
Prelude> (fmap . fmap) replaceWithP lms
[Nothing,Just 'p',Just 'p']
Prelude> (fmap . fmap . fmap) replaceWithP lms
[Nothing,Just "ppp",Just "ppp"]
```

#### Functor laws

- Idenity

```haskell
fmap id == id
```

- Composition

```haskell
fmap (f . g) = fmap f . fmap g
```

- Preserve structure

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```
