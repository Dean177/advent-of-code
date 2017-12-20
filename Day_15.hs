{-# LANGUAGE BangPatterns #-}
module Day_15 where

import Data.Bits ((.&.))
import qualified Data.Char as Char
import Data.Function (on)
import Data.List
import Data.Word
import qualified Data.Vector as V
import Numeric (showIntAtBase)

lowest16Bits :: Word64 -> Word64
lowest16Bits = (.&. 0xffff)

hasEqual16Bits :: (Word64, Word64) -> Bool
hasEqual16Bits = uncurry ((==) `on` lowest16Bits)

genA :: Word64 -> Word64
genA x = x * 16807 `mod` 2147483647

genB :: Word64 -> Word64
genB x = x * 48271 `mod` 2147483647

type GenStep = (Word64, Word64) -> (Word64, Word64)

generate :: GenStep
generate (a, b) = (genA a, genB b)

count :: GenStep -> (Word64, Word64) -> Int -> Int
count gen = count' 0 where
  count' :: Int -> (Word64, Word64) -> Int -> Int
  count' !k x 0 = k
  count' !k x n =
    count' (if hasEqual16Bits y then k + 1 else k) y (n - 1)
      where y = gen x

part1 :: (Word64, Word64) -> Int
part1 input =
  count generate input 40000000

genA', genB' :: Word64 -> Word64
genA' x =
  let y = genA x
  in if y .&. 3 == 0 then y else genA' y
genB' x =
  let y = genB x
  in if y .&. 7 == 0 then y else genB' y

generate' :: (Word64, Word64) -> (Word64, Word64)
generate' (a, b) = (genA' a, genB' b)

part2 :: (Word64, Word64) -> Int
part2 input =
  count generate' input 5000000

main = do
  print $ part1 (0,0)
  print $ part2 (0,0)
