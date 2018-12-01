module Day_14 where

import Data.Bits (finiteBitSize, popCount, testBit)
import Data.Graph (components, graphFromEdges)
import Data.List (zip3, zip4)
import Data.Tree (flatten)
import Data.Word (Word8)
import Day_10 (knotHash)

zipWithIndex = zip [0..]

grid :: String -> [[Word8]]
grid input = fmap knotHash $ ((input ++) . ('-':) . show) <$> [0..127]

part1 :: String -> Int
part1 = sum . fmap popCount . concat . grid

bits :: Word8 -> [Bool]
bits b = testBit b <$> reverse [0 .. finiteBitSize b - 1]

type Region = [(Int, Int)]
groupedBits :: [[Word8]] -> [Region]
groupedBits input = fmap vertex . flatten <$> components graph
  where
    bitgrid :: [[Bool]]
    bitgrid = concatMap bits <$> input
    edges = [
      ((), (x, y), [(x - 1, y) | isConnectedLeft] ++ [(x, y - 1) | isConnectedAbove]) |
        (y, currentRow, previousRow) <-
          zip3 [0..] bitgrid $ repeat False : bitgrid,
        -- The 'True' is a pattern match to filter 'unfilled' bits
        (x, True, isConnectedAbove, isConnectedLeft) <-
          zip4 [0..] currentRow previousRow $ False : currentRow
      ]
    (graph, findVertex, _) = graphFromEdges edges
    vertex v = let (_, xy, _) = findVertex v in xy

part2 :: String -> Int
part2 = length . groupedBits . grid

puzzleInput = "uugsqrei"
main = do
  print $ part1 puzzleInput
  print $ part2 puzzleInput
