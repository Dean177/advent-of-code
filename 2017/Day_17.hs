{-# LANGUAGE BangPatterns #-}
module Day_17 where

steps = 304

insertAt n x l = take n l ++ [x] ++ drop n l

part1 :: Int -> Int -> [Int] -> Int
part1 2018 position list = list !! (position + 1)
part1 value position list =
  let !nextPosition  = (position + steps `mod` value + 1) `mod` value
  in part1 (value + 1) nextPosition (insertAt nextPosition value list)

part2 :: Int -> Int -> Int -> Int
part2 50000001 _ valueAtOne = valueAtOne
part2 value position valueAtOne =
  let !nextPosition  = (position + steps `mod` value + 1) `mod` value
  in part2 (value + 1) nextPosition (if nextPosition == 0 then value else valueAtOne)

main = do
  print $ part1 1 1 [0]
  print $ part2  1 1 1
  return ()
