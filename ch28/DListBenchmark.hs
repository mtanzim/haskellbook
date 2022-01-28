
module DListBenchmark where
import Criterion.Main

-- http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL $ const []

singleton :: a -> DList a
singleton a = DL $ const [a]

toList :: DList a -> [a]
toList dl = unDL dl []

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons a dl = DL $ const $ unDL dl [a]

append :: DList a -> DList a -> DList a
append dl1 dl2 = DL $ unDL dl1 . unDL dl2

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc dl a = append dl (singleton a)

schlemiel :: Int -> [Int]
schlemiel i =
  go i []
  where
    go 0 xs = xs
    go n xs = go (n -1) [n] ++ xs

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n -1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456,
      bench "concat dlist" $ whnf constructDlist 123456
    ]