module Day_2 where

import Data.List
import Data.Maybe

parseInt = read :: String -> Int
parseLine = map parseInt
parse input = parseLine . words <$> lines input

-- Part 1
checkNum row = maximum row - minimum row
checkSum rows = foldl' (+) 0 $ map checkNum rows


-- Part 2
isEvenDivisor a b = a `mod` b == 0
sortDescending = sortBy (flip compare)

findDivisor :: Int -> [Int] -> (Int, Maybe Int)
findDivisor x xs = (x, find (isEvenDivisor x) xs)

findTopDivisor :: [Int] -> [(Int, Maybe Int)]
findTopDivisor [x] = [(x, Nothing)]
findTopDivisor (x:xs) = findDivisor x xs : findTopDivisor xs

checkSum2 row = head $ filter (not . null . snd)
  (findTopDivisor sortedRow)
    where sortedRow = sortDescending row

divPair (x, maybeY) = x `div` fromMaybe 0 maybeY
