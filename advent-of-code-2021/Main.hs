module Main where

import Day1 (day1Main)
import Day2 (day2Main)
import Day3 (day3Main)
import Day4 (day4Main)
import Day5 (day5Main)
import Day6 (day6Main)
import Day7 (day7Main)
import Day8 (day8Main)

main :: IO ()
main = do
  putStrLn "What day?"
  whichDay <- getLine
  case whichDay of
    "1" -> day1Main
    "2" -> day2Main
    "3" -> day3Main
    "4" -> day4Main
    "5" -> day5Main
    "6" -> day6Main
    "7" -> day7Main
    "8" -> day8Main
    _ -> putStrLn "Invalid entry"