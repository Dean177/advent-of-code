module Day_11 where

import Data.List.Split (splitOn)

step (x, y, z) "n" = (x, y + 1, z - 1)
step (x, y, z) "s" = (x, y - 1, z + 1)
step (x, y, z) "ne" = (x + 1, y, z - 1)
step (x, y, z) "sw" = (x - 1, y, z + 1)
step (x, y, z) "nw" = (x - 1, y + 1, z)
step (x, y, z) "se" = (x + 1, y - 1, z)

distance (x, y, z) = (abs x + abs y + abs z) `div` 2

main = do
  input <- splitOn "," <$> readFile "./Day_11.txt"
  print $ distance $ foldl step (0, 0, 0) input
  -- Part 2
  print $ maximum $ distance <$> scanl step (0,0,0) input
  return ()
