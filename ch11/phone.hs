module Phone where

import Control.Arrow (Arrow (second))
import Data.Char
import System.Posix.Terminal.ByteString (sendBreak)

type Digit = Char

type Presses = Int

type PossibleChars = String

-- data T9Press = T9press Button

data PhoneButton = PhoneButton Digit PossibleChars

type DaPhone = [PhoneButton]

-- TODO: this should be a hash map or similar
phone :: DaPhone
phone = [PhoneButton '*' "", PhoneButton '#' ".,", PhoneButton '0' " 0", PhoneButton '1' "1", PhoneButton '2' "abc2", PhoneButton '3' "def3", PhoneButton '4' "ghi4", PhoneButton '5' "jkl5", PhoneButton '6' "mno6", PhoneButton '7' "pqrs", PhoneButton '8' "tuv", PhoneButton '9' "wxyz"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = if isUpper c then ('*', 1) : [digit p (toLower c)] else [digit p c]
  where
    digit ((PhoneButton d chars) : tail) c' = if elem c' chars then (d, getPos chars 0 c') else digit tail c'
    digit [] _ = ('!', -1)
    getPos (curChar : rest) curIdx c'' = if curChar == c'' then curIdx + 1 else getPos rest (curIdx + 1) c''
    getPos [] _ _ = -2

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p sentence = foldr ((++)) [] (map (\c -> reverseTaps p c) sentence)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = (foldr ((+)) 0 . map (\(d, p) -> p))

counts :: Eq a => a -> [a] -> Int
counts c = length . (filter (\x -> x == c))

countsList :: String -> [(Char, Int)]
countsList s = map (\c -> (c, counts c s)) s

countsMax :: [(Char, Int)] -> (Char, Int)
countsMax xs = foldr (\acc cur -> if (snd cur) > (snd acc) then cur else acc) (head xs) xs

mostPopularLetter :: String -> Char
mostPopularLetter = (fst . countsMax . countsList)
