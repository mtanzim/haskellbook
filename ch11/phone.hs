module Phone where

type Digit = Char

type Presses = Int

type PossibleChars = String

-- data T9Press = T9press Button

data PhoneButton = PhoneButton Digit PossibleChars

type DaPhone = [PhoneButton]

phone :: DaPhone
phone = [PhoneButton '2' "abc2", PhoneButton '3' "def3", PhoneButton '4' "ghi4", PhoneButton '5' "jkl5", PhoneButton '6' "mno6"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = [digit p]
  where
    digit ((PhoneButton d chars) : tail) = if elem c chars then (d, getPos chars 0) else digit tail
    digit [] = ('!', -1)
    getPos (curChar : rest) curIdx = if curChar == c then curIdx + 1 else getPos rest curIdx + 1
    getPos [] _ = -1
