import Control.Applicative (liftA2)
import InChapter (Reader (Reader))

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

chris :: Person
chris = Person (HumanName "Chris Rock") (DogName "Ppau") (Address "San Anto")

pers :: Person
pers = Person (HumanName "Percy") (DogName "Dercy") (Address "San Hose")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- Note how p is the same arg to both functions

-- * Main> :t Dog <$> dogName

-- Dog <$> dogName :: Person -> Address -> Dog

-- * Main> :t Dog <$> dogName <*> address

-- Dog <$> dogName <*> address :: Person -> Dog

-- * Main>

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- * Main> :t liftA2 Dog dogName

-- liftA2 Dog dogName :: (Person -> Address) -> Person -> Dog

-- * Main> :t liftA2 Dog dogName address

-- liftA2 Dog dogName address :: Person -> Dog
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader (\p -> Dog (dogName p) (address p))
