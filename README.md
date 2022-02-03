# haskellbook

Exercises and content for the [haskellbook](https://haskellbook.com/)

Advent of code 2021 was moved [here](https://github.com/mtanzim/advent-of-code-2021)

## Why?

- Curiosity around functional programming

## Notes on critical concepts

### Monoids and Semigroup

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
mappend x mempty = x
mappend mempty x = x
```

In Haskell, we define a type class for monoid as follows:

```haskell
class Semigroup m => Monoid m where mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

Taking a concrete example for lists in Haskell:

```haskell
instance Semigroup [a] where
  (<>) = (++)
instance Monoid [a] where
  mempty = []
```

Monoid laws can be highlighted as follows:

```haskell
-- left identity
mappend mempty x == x
-- right identity
mappend x mempty == x
-- associativity
mappend x (mappend y z) == mappend (mappend x y) z
mconcat == foldr mappend mempty
```

A **Semigroup** is only a **binary associative** operation _without_ an indentity.

```haskell
class Semigroup a where
(<>) :: a -> a -> a
```
