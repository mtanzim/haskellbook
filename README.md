# haskellbook

Exercises and content for the [haskellbook](https://haskellbook.com/)

## Why?

- Curiosity around functional programming

## Related Efforts

- [Advent of code 2021 in Haskell](https://github.com/mtanzim/advent-of-code-2021)
- [Haskell MOOC from U of Glasgow](https://github.com/mtanzim/fp-haskell)
- [FP with Scala](https://github.com/mtanzim/fp-scala)
- [FP with Standard ML](https://github.com/mtanzim/prog-lang-a)
- [FP with Racket](https://github.com/mtanzim/prog-lang-b)

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

- Note that for a type to have a `Functor` instance, it must be of kind `* -> *` or higher
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

### Applicative

[Chapter exercises](./ch17)

- Applicatives are monoidal functors
- With functors we apply a function inside a structure, with applicatives, the function itself is inside of a structure
- The monoid part is responsible to merging the structures together
- For example, let's see a list of functions `ap`ped to a list of integers

```ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> fns = [(+1), (*2)]
Prelude> nums = [1,2,3]
Prelude> fns <*> nums
[2,3,4,2,4,6]
Prelude>
```

- Note how the results of `(+1)` being `[2,3,4]` are concatenated (monoidal operation `mappend`) with the results of `(*2)` being `[2,4,6]`

- Defining applicative as a type class:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

- Note how every type that can have an applicative instance must also have a functor instance
- The `pure` function _lifts_ something into the functorial/applicative structure
- `<*>` is often called `ap` or `apply`
- Noting the similarities between `<*>` and `fmap` or `<$>`:

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

- The above types demonstrates:
  > with applicatives, the function itself is inside of a structure

#### Where are the monoids?

- We claimed applicatives are **monoidal** functors
- Firstly, note the types of the following

```haskell
($) :: (a -> b) -> a -> b
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

- Looking closer at the `<*>`, we we notice there are 2 arguments: `f (a -> b)` and `f a`, or put in another way:

```text
:: f (a -> b) -> f a -> f b
f           f     f
(a -> b)    a     b
```

- Now looking at the definition of `mappend`: `mappend :: Monoid a => a -> a -> a`
- We can see that we have `Monoid` for our structure (`f`) and function application for the values

```text
mappend   :: f                f           f
$         :: (a -> b)         a           b
(<*>)     :: f (a -> b) ->    f a ->      f b
```
