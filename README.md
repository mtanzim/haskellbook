# haskellbook

Exercises and content for the [haskellbook](https://haskellbook.com/)

## Why?

- Curiosity around functional programming

## Related Efforts

- [Advent of code 2021 in Haskell](https://github.com/mtanzim/advent-of-code-2021)
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
mappend [] [1..5]
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

- We can see the monoid aspect of applicatives in action with Haskell tuples

```ghci
Prelude> fmap (+1) ("blah", 0)
("blah",1)
```

- Whereas with applicatives:

```ghci
Prelude> ("Woo", (+1)) <*> (" Hoo!", 0)
("Woo Hoo!", 1)

Prelude> (Sum 2, (+1)) <*> (Sum 0, 0)
(Sum {getSum = 2},1)

Prelude> (Product 3, (+9))<*>(Product 2, 8)
(Product {getProduct = 6},17)
```

- This is seen in the instance definition themselves, ie for tuples:

```haskell
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (a, b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
```

#### Usages of applicatives

```text
[(+1), (*2)] <*> [2, 4]
[ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
```

```ghci
Prelude> (,) <$> [1, 2] <*> [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
```

- The above can be thought of as:

```text
Prelude> (,) <$> [1, 2] <*> [3, 4]

fmap the (,) over the first list:
[(1, ), (2, )] <*> [3, 4]

Then, we apply the first list to the second:

[(1,3),(1,4),(2,3),(2,4)]
```

- `liftA2` gives us a more terse way of expressing the above

```ghci
Prelude> liftA2 (,) [1, 2] [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
```

- Similarly

```ghci
Prelude> (+) <$> [1, 2] <*> [3, 5]
[4,6,5,7]
Prelude> liftA2 (+) [1, 2] [3, 5]
[4,6,5,7]
Prelude> max <$> [1, 2] <*> [1, 4]
[1,4,2,4]
Prelude> liftA2 max [1, 2] [1, 4]
[1,4,2,4]
```

#### Applicative laws

- Identity

`pure id <*> v = v`

- Composition

`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

- Homomorphism

`pure f <*> pure x = pure (f x)`

ie:

```text
pure (+1) <*> pure 1
pure ((+1) 1)
```

- Interchange

`u <*> pure y = pure ($ y) <*> u`

### Monad

[Chapter exercises](./ch18)

- Monads are applicative functors
- As a type class

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

- Note the chain of dependency between functors, applicatives and monads: `Functor -> Applicative -> Monad`
- In other words, functor and applicative can be derived from a monad, one law demonstrating this is: `fmap f xs = xs >>= return . f`
- `return` is the same as pure, it simply lifts a value into context
- `>>` aka Mr. Pointy aka the sequencing operator sequences two actions while dismissing the result
- `>>=` is the `bind` operator, and it is what makes monads special
- Let's reflect on the similarities between the key operations between functors, applicatives and monads:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
<*> :: Applicative f => f (a -> b) -> f a -> f b
>>= :: Monad f => f a -> (a -> f b) -> f b
```

- To develop and intuition for the essence of monads,let's take an example using map on a function of type `(a -> f b)`

```haskell
fmap :: Functor f => (a -> f b) ->  a -> f (f b)
```

- Seeing the above with lists:

```ghci
Prelude> andOne x = [x, 1]
Prelude> andOne 10
[10,1]
Prelude> :t fmap andOne [4, 5, 6]
fmap andOne [4, 5, 6] :: Num t => [[t]]
Prelude> fmap andOne [4, 5, 6]
[[4,1],[5,1],[6,1]]
```

- Noting that now we have a list of lists, how do we flatten it? We use `concat`

```ghci
Prelude> concat $ fmap andOne [4, 5, 6]
[4,1,5,1,6,1]
```

- Monad in ways is the generalization of the `concat`, which leads us to the monad's join function:

```haskell
join :: Monad m => m (m a) -> m a
-- compare
concat :: [[a]] -> [a]
```

- Note that the `join` is what sets monads apart, this ability to peel away the extra embedded structure that is of the same type as the outer structure

- Seeing the type signature for bind, we also note that bind is simply `fmap` composed with `join`:

```haskell
-- keep in mind this is >>= flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind = join . fmap
```

- Note that the `do` syntax in Haskell is sugar for sequencing and binding monads

```haskell
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >> putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn
```

- Note how the `do` syntax helps avoid nesting
- The style is also reminiscent of imperative programming, but we are still programming in a purely functional way

```haskell
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)
```

- [See example of the Maybe monad in use](./ch18/Cow.hs)
- [See example of the Either monad in use](./ch18/SoftwareShop.hs)

#### Laws

- Identity: `return` should not perform an operations

```text
-- right identity
m >>= return = m
-- left identity
return x >>= f = f x
```

- Associativity: `(m >>= f) >>= g = m >>= (\x-> f x >>= g)`

#### Composition

- To compose monads, we need **Kleisli Composition**

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(>>=) :: Monad m => m a -> (a -> m b) -> m b
-- the order is flipped to match >>=
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
flip (.) :: (a -> b) -> (b -> c) -> a -> c
```

- Taking an example

```haskell

import Control.Monad ((>=>))
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine
readM :: Read a => String -> IO a
readM = return . read
getAge :: String -> IO Int
getAge = sayHi >=> readM
askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "
```

### Foldable

[Chapter exercises](./ch20)

- A list fold reduces the values inside a list to a summary value
- This is done by recursively applying some function
- The folding function is always dependent on some `Monoid` instances
- Seeing the class definition, a Foldable instance must define either `foldMap` or `foldr` to be minimally complete

```haskell
class Foldable t where
{-# MINIMAL foldMap | foldr #-}
```

- Also, note: `class Foldable (t :: * -> *) where`, indicating `t` must be a higher kinded type
- Seeing the overall class definition:

```haskell
class Foldable (t :: * -> *) where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m=> (a -> m) -> t a -> m
```

- `fold` allows us to combine elements inside a Foldable structure using the defined Monoids
- `foldMap` on the other hand **first maps each element of the structure
  to a Monoid**, and then combines the results
- Note the following examples to observe the differences b/w `foldr`, `fold` and `foldMap`

```ghci
Prelude> foldr (+) 0 [1..5]
15
Prelude> fold (+) [1, 2, 3, 4, 5]
-- error message resulting from incorrect
-- number of arguments

Prelude> xs = map Sum [1..5]
Prelude> fold xs
Sum {getSum = 15}


Prelude> :{
*Main| let xs :: [Sum Integer]
*Main|     xs = [1, 2, 3, 4, 5]
*Main| :}
Prelude> fold xs
Sum {getSum = 15}
```

- In some cases, the compiler can figure out the monoid instance and doesn't need the explicit typing, for example with strings:

```ghci
Prelude> foldr (++) "" ["hello", " julie"]
"hello julie"
```

- Similarly, one can use a `foldMap` to specify the function that will convert the elements to something with a non ambiguous monoid instance

```ghci
Prelude> foldMap Sum [1, 2, 3, 4]
Sum {getSum = 10}
Prelude> foldMap Product [1, 2, 3, 4]
Product {getProduct = 24}

```

- Note that `Sum` and `Product` are the functions/data constructors that disambiguate the monoid instance for integers, but `foldMap` can also be used as follows:

```ghci
Prelude> xs = map Sum [1..3]
Prelude> foldMap (*5) xs
Sum {getSum = 30}
-- 5 + 10 + 15
30
```

- Let's look at some examples of Foldable instances

```haskell
instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
```

```ghci
Prelude> foldr (*) 1 (Identity 5)
5
Prelude> foldl (*) 5 (Identity 5)
25
```

```haskell
-- Using a fake maybe type
instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
```

```ghci
Prelude> foldr (+) 1 Nothing
1

Prelude> fm = foldMap (+1)
Prelude> fm Nothing :: Sum Integer
Sum {getSum = 0}
```

- Note that in the above cases, the fold functions are not used to combine values, but simply consume the values from within their structures

### Traversable

[Chapter exercises](./ch21)

- Functors transform values within a structure
- Applicatives transform values with within a structure where the transformer function is also within a structure
- **Traversable allows us to process values embedded in a structure as if they existed in a sequential order**

> Traversable allows you to transform elements inside a structure like a functor, producing applicative effects along the way, and lift those potentially multiple instances of applicative structure outside of the traversable structure. It is commonly described as a way to traverse a data structure, mapping a function inside a structure while accumulating applicative contexts in the process.

- Seeing the traversable type class definition:

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
```

- `traverse` maps each element in a structure to an action, evaluates the actions left to right, and then collects the results
- Comparing `traverse` to `fmap`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

myData :: [String]
myFunc :: String -> IO Record
wrong :: [IO Record]
wrong = fmap myFunc myData
right :: IO [Record]
right = traverse myFunc myData
```

-`traverse`s counterpart is `sequenceA`; note the minimal instance as `travserse` and `sequenceA` can be defined in terms of the other

```haskell
-- | Evaluate each action in the
-- structure from left to right,
-- and collect the results.
sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceA = traverse id
{-# MINIMAL traverse | sequenceA #-}
```

- Let's see `sequenceA` in more detail
- As seen in the type signature, `sequenceA` **only** flips two contexts or structure

```ghci
Prelude> xs = [Just 1, Just 2, Just 3]
Prelude> sequenceA xs
Just [1,2,3]
Prelude> xsn = [Just 1, Just 2, Nothing]
Prelude> sequenceA xsn
Nothing
Prelude> fmap sum $ sequenceA xs
Just 6
Prelude> fmap product (sequenceA xsn)
Nothing
```

- In the first two example, we are simply flipping around the list, and `Maybe` contexts
- Note in the second set of examples, we can lift a function (`sum` in this case) into the `Maybe` context

- Looking at `traverse` now, keeping in mind `traverse f = sequenceA . fmap f`

```ghci
Prelude> fmap Just [1, 2, 3]
[Just 1,Just 2,Just 3]
Prelude> sequenceA $ fmap Just [1, 2, 3]
Just [1,2,3]
Prelude> sequenceA . fmap Just $ [1, 2, 3]
Just [1,2,3]
Prelude> traverse Just [1, 2, 3]
Just [1,2,3]
```

- In general, we use traversable when we need to flip around two type constructors, or map a function and hen flip around the constructors
- Note also that `Traversable` is stronger than `Functor` and `Foldable`, therefore, we can recover `Functor` and `Foldable` from `Traversable`, similar to how `Functor` and `Applicative` can be recovered from `Monad`
- See the [chapter exercises](./ch21/ChapterExercises.hs) for examples of `Traversable` instances

#### Traversable laws

`traverse` must satisfy:

- Naturality: `t . traverse f = traverse (t . f)`
- Identity: `traverse Identity = Identity`
- Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

`sequenceA` must satisfy:

- Naturality: `t . sequenceA = sequenceA . fmap t`
- Identity: `sequenceA . fmap Identity = Identity`
- Composition: `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`
