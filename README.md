# haskellbook

Exercises and content for the [Haskell programming from first principles](https://haskellbook.com/)

## Why?

- Curiosity around functional programming concepts and purely functional programming

## Related efforts

- [Advent of code 2021 in Haskell](https://github.com/mtanzim/advent-of-code-2021)
- [FP with Scala](https://github.com/mtanzim/fp-scala)
- [FP with Standard ML](https://github.com/mtanzim/prog-lang-a)
- [FP with Racket](https://github.com/mtanzim/prog-lang-b)

## Related resources

- <http://learnyouahaskell.com/chapters>
- <https://mostly-adequate.gitbook.io/mostly-adequate-guide/>

## Repositories that helped along the way

- <https://github.com/andrewMacmurray/haskell-book-solutions>
- <https://github.com/lisss/yaths>
- <https://github.com/johnchandlerburnham/hpfp>

## Notes on critical concepts

### Type classes

[Chapter exercises](./ch01-ch06/typeClasses.hs)

- In Haskell, a declaration of a type defines how that type is constructed
- A declaration of a type class on the other hand defines how a set of types are **consumed**
- Type classes allow for generalizations over a set of types in order to define and execute a set of features for those types
- One example, it is very useful to be able to test values for equality. So, any type that implements the `Eq` type class, it's values can be tested for equality
- As long as a datatype implements or instantiates the `Eq` type class, the functions `==` and `/=` can be used
- Looking at an example datatype, `Bool` instantiates the following type classes:

```ghci
Prelude> :info Bool
data Bool = False | True
instance Eq Bool
instance Ord Bool
instance Show Bool
instance Read Bool
instance Enum Bool
instance Bounded Bool
```

- Looking at an example type type class:

```ghci
Prelude> :info Eq
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

- The above states, for any type `a` to implement the `Eq` type class, it must define the `==` and `\=` functions that follow the required signature

- Attempting to write a type class instance of a new data type, we arrive at:

```haskell
data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True
```

- Note that `\=` can be derived from `==` and thus its definition is not required for the minimal implementation

- Looking at another example:

```haskell
data Identity a = Identity a
instance Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```

- Type classes are automatically dispatched by type

### Algebraic datatypes

[Chapter exercises](./ch11)

- A type can be thought of as an enumeration of constructors that take zero or more arguments
- Haskell offers the following: sum types, product types (including record syntax), type aliases, and a special datatype called `newtype` (which will not be covered in the notes)
- Haskell has 2 types of constructors: type constructors and data constructors
- Type constructors are used only at the type level: in type signatures, and type class declarations/instances
- Data constructors create values at term level (values that can be interacted with at run time)
- Type and data constructors not taking any arguments are **constants**, ie: `Bool` is a type constant, and `True` and `False` are data constants
- However, we need to allow different types or amounts of data to be store in our data types. In these times, the type and data constructors are **parametrized**
- In these cases, the constructors are like functions, as they must be applied to become a concrete type of value

```haskell
data Trivial = Trivial'
data UnaryTypeCon a = UnaryValueCon a
```

- In the above, `Trivial` is a constant value at term level; it takes no arguments and this is called **nullary** or a **type constant**
- `Trivial'` is a constant **value** as it exists at term level, or run time space
- `UnaryTypeCon` is a **type constructor** with one argument, awaiting a **type constant** to be applied to it
- `UnaryValueCon` is a **data constructor** awaiting a value to be applied to it
- The same is true of list datatype
- At the type level, we have `a : [a]`, where `a` is a variable
- At runtime, when a type of value can be applied, it can become concrete, ie: `[Char]` or `[Int]`
- This leads us to the idea of `kinds`, these are the types of the type constructors
- Kinds are not types until they are fully applied
- The fully applied kind is denoted as `*`, where is the kind `* -> *` is awaiting a single `*` to be applied
- Anything that is awaiting application is a **higher kinded datatype**, an example is lists as seen above
- Let's see some examples

```haskell
-- identical to (a, b, c, d)
data Silly a b c d =
  MkSilly a b c d deriving Show
```

```ghci
Prelude> :kind Silly
Silly :: * -> * -> * -> * -> *

Prelude> :kind Silly Int
Silly Int :: * -> * -> * -> *

Prelude> :kind Silly Int String Bool String
Silly Int String Bool String :: *
```

- The number of arguments a type or data constructor takes is its **arity**, ie: nullary, unary

- The number of possible values a datatype can hold is its **cardinality**
- For example, `Bool` has a cardinality of 2 (`True | False`), whereas `Int8` has a cardinality of 256 (-128 to 127)

#### Sum types

- An example : `data Bool = False | True`
- `|` represents a logical disjunction aka `or`, this is the `sum` in algebraic datatypes
- To know the cardinality of sum types, we _add_ the cardinalities of it data constructors; so for `Bool`, it is 2, `True` and `False`

#### Product types

- A product type’s cardinality is the product of the cardinalities of its inhabitants
- A product type expresses `and`
- Tuples are an example of a product type: `(,) :: a -> b -> (a, b)`
- It allows us to have two kinds of data, each with their own type (or not)
- Following is an example of a product type with the record syntax

```haskell
data Person = Person { name :: String, age :: Int }
```

- You may combine sum and product types, for example

```haskell
data Gender = Male | Female
data Person = Person { name :: String, age :: Int, gender: Gender }
```

- The function type (`a -> b`) has exponential cardinality, ie: `(Bool -> Bool)` has the cardinality of `2 ^ 2`

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

- Identity

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

- Monad in ways is the generalization of the `concat`, which leads us to the monad's <code>join</code> function:

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
  putStrLn ("y hello thar: " ++ name)

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

-`traverse`s counterpart is `sequenceA`; note the minimal instance as `traverse` and `sequenceA` can be defined in terms of the other

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

### Monad Transformers

[Chapter exercises- type composition](./ch25)
[Chapter exercises- monad transformer usage](./ch26)

Note: this concept is very dense, and cannot be summarized effectively (at least not by me, yet anyways). Refer back to the book itself and other resources as needed. Following is a decent, terse overview: [Monad Transformers Explained](https://wiki.haskell.org/Monad_Transformers_Explained)

- The issue with monads, unlike functors and applicatives, is that one cannot guarantee getting a monad out of putting two other monads together
- Hence, to when we need to compose two monads, we need monad transformers
- The monad transformer is a type constructor that takes a `Monad` as an argument and returns a `Monad` as a result
- The essence of the issue of composing two monads is that it is impossible to `join` two unknown monads.
- To make the `join` happen, we need to reduce polymorphism an make one of the monads concrete. The other can stay as a variable
- Note the [`IdentityT` monad transformer](./ch25/IdentityT.hs) as an example
- Quoting the book: note the following, critical pattern

<blockquote>
Transformers are bearers of single-type concrete information that let you create ever-bigger monads, in a sense. Nesting such as:

<code>(Monad m) => m (m a)</code>

Is addressed by join already. We use transformers when we want a <code>>>=</code> operation over f and g of different types (but both have Monad instances). You have to create new types called monad transformers and write Monad instances for those types to have a way of dealing with the extra structure generated.

The general pattern is this: You want to compose two polymorphic types, f and g, that each have a Monad instance. But you’ll end up with this pattern:

<code>f (g (f b))</code>

Monad’s bind can’t join those types, not with that intervening g. So you need to get to this:

<code>f (f b)</code>

You won’t be able to unless you have some way of folding the g in the middle. You can’t do that with Monad. The essence of Monad is <code>join</code>, but here you have only one bit of g structure, not g (g ...), so that’s not enough. The straightforward thing to do is to make g concrete. With concrete type information for the inner bit of structure, we can fold out the g and get on with it. The good news is that transformers don’t require f to be concrete; f can remain polymorphic, so long as it has a Monad instance, and therefore we only need to write a transformer once for each type.

</blockquote>
