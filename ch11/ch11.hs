module Ch11 where

import Data.Int

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Size = Size Double deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 2000)

clownCar :: Vehicle
clownCar = Car Tata (Price 700)

doge :: Vehicle
doge = Plane PapuAir (Size 2.5)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = foldr (\a b -> isCar a && b) True

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

-- Pity the Bool
-- cardinality 4
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- cardinality 256 + 2
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- Gardener
type Gardener = String

-- data FlowerType
--   = Gardenins
--   | Daisy
--   | Rose
--   | Lilac
--   deriving (Show)

-- data Garden = Garden Gardener FlowerType deriving (Show)

-- Normal Form
data Gardenis = Gardenis deriving (Show)

data Daisy = Daisy deriving (Show)

data Rose = Rose deriving (Show)

data Lilac = Lilac deriving (Show)

-- Normal form
data Garden = GardenisGarden Gardener Gardenis | DaisyGarden Gardener Daisy | RoseGarden Gardener Rose | LilacGarden Gardener Lilac deriving (Show)
