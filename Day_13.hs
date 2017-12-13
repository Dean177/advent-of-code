module Day_13 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

layer :: Parser (Int, Int)
layer = do
  depth <- read <$> many digitChar
  string ": "
  range <- read <$> many digitChar
  return (depth, range)

layers = layer `sepBy` char '\n'

isCaught (depth, range) = depth `mod` (2 * (range - 1)) == 0

severity (depth, range) = depth * range

isCaughtWithDelay :: [(Int, Int)] -> Int -> Bool
isCaughtWithDelay depthRanges delay = or $ caught delay <$> depthRanges
  where caught delay (depth, range) = isCaught (depth + delay, range)

main = do
  (Right puzzleInput) <- runParser layers "Packet scanner" <$> readFile "./Day_13.txt"--testInput
  print $ sum $ severity <$> filter isCaught puzzleInput

  -- Part 2
  print $ head $ filter (not . isCaughtWithDelay puzzleInput) [0..]
