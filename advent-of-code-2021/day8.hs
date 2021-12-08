module Day8 where

import Data.List.Split

day8TestInput :: IO [[String]]
day8TestInput = do
  inputs <- readFile "day8TestInput.txt"
  return ((map (splitOn " | ") . lines) inputs)

testInput :: [[String]]
testInput = [["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb", "fdgacbe cefdb cefbgd gcbe"], ["edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec", "fcgedb cgb dgebacf gc"], ["fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef", "cg cg fdcagb cbg"], ["fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega", "efabcd cedba gadfec cb"], ["aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga", "gecf egdcabf bgf bfgea"], ["fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf", "gebdcfa ecba ca fadegcb"], ["dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf", "cefg dcbef fcge gbcadfe"], ["bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd", "ed bcgafe cdgba cbgef"], ["egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg", "gbdfcae bgc cg cgb"], ["gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc", "fgae cfgab fg bagce"]]

onlyDigitOutputs :: [[String]] -> [String]
onlyDigitOutputs = map last

wordsInInput :: [[String]] -> [String]
wordsInInput = concat . map words . onlyDigitOutputs

countInstancesOfSimpleDigits :: [[String]] -> Int
countInstancesOfSimpleDigits = length . filter fn . wordsInInput
  where
    fn word = case length word of
      2 -> True
      4 -> True
      3 -> True
      7 -> True
      _ -> False

main :: IO ()
main = do
    input <- day8TestInput
    print (countInstancesOfSimpleDigits input)