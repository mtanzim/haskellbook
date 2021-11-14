import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

isWordPalindrome :: Eq a => [a] -> Bool
isWordPalindrome w = reverse w == w

checkPalindrome :: String -> Bool
checkPalindrome =
  isWordPalindrome . concat . words . map toLower

palindrome :: IO ()
palindrome = forever $ do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Type in a word to check if it's a palindrome: "
  line1 <- getLine
  case checkPalindrome line1 of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess