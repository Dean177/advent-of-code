module Day_4 where
import Data.List 
import Data.List.Unique

parse = map words . lines

-- Part 1
validPassphrase = null . repeat
solve = length . filter validPassphrase

-- Part 2
validLine line = validPassphrase $ map sort line
solve_2 = length . filter validLine
