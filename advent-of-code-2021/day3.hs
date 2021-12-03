module AdventDay3 where

testInput :: [String]
testInput = ["00100", "11110", "10110"]

numCharsPerEntry = length (testInput !! 0)

curCol curIdx maxIdx input = if curIdx > maxIdx then [] else map (\row -> row !! curIdx) input

main curIdx = curCol curIdx numCharsPerEntry testInput
