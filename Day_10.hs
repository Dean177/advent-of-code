module Day_10 where

import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.Vector as V
import Numeric (showHex, showIntAtBase)
import Text.Printf (printf)

input = V.fromList [0..255]
lengths = [88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205]
puzzleInput = "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205"

wrap :: Int -> Int
wrap i = i `mod` 256

twist :: Int -> Int -> V.Vector Int -> V.Vector Int
twist from twistLength vec
  | twistLength == 1 = vec
  | from + twistLength <= 256 =
      V.slice 0 from vec V.++
      V.reverse (V.slice from twistLength vec) V.++
      V.slice (from + twistLength) (V.length vec - from - twistLength) vec
  | otherwise = newFrontSlice V.++ newMidSlice V.++ newEndSlice
    where
      vecLen = V.length vec
      endLength = vecLen - from
      endSlice = V.slice from endLength vec
      frontLength = wrap (twistLength - (vecLen - from))
      frontSlice = V.slice 0 frontLength vec
      reversedSegment = V.reverse (endSlice V.++ frontSlice)
      reversedSegmentLength = V.length reversedSegment
      newFrontSlice = V.slice (reversedSegmentLength - frontLength) frontLength reversedSegment
      newMidSlice = V.slice (wrap $ from + twistLength) (vecLen - twistLength) vec
      newEndSlice = V.slice 0 endLength reversedSegment

hash :: Int -> Int -> V.Vector Int -> [Int] -> (Int, Int, V.Vector Int)
hash skipSize currentIndex vector [] = (skipSize, currentIndex, vector)
hash skipSize currentIndex vector (l:ls) =
  hash
    (skipSize + 1)
    (wrap $ currentIndex + l + skipSize)
    (twist currentIndex l vector)
    ls

runRounds :: [Int] -> Int -> (Int, Int, V.Vector Int) -> (Int, Int, V.Vector Int)
runRounds _ 0 sspv = sspv
runRounds ls n (skipSize, currentIndex, vec) =
  runRounds ls (n - 1) (hash skipSize currentIndex vec ls)

condenseBlock :: V.Vector Int -> Int
condenseBlock = foldl Bits.xor 0

condense :: V.Vector Int -> V.Vector Int -> V.Vector Int
condense ys xs
  | V.null xs = ys
  | otherwise = let currentBlock = condenseBlock (V.take 16 xs) in
    condense (ys `V.snoc` currentBlock) (V.drop 16 xs)

toHex :: Int -> String
toHex n
  | length hexNum == 1 = "0" ++ hexNum
  | otherwise = hexNum
  where hexNum = showHex n ""

parseLengths :: String -> [Int]
parseLengths input = (Char.ord <$> input) ++ [17, 31, 73, 47, 23]

toHexString :: V.Vector Int -> String
toHexString ints = concat $ toHex <$> ints

main = do
  -- print $ condense V.empty $ V.fromList [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22]
  -- print $  [64, 7, 255]
  let lengths = parseLengths puzzleInput :: [Int]
  let initialValues = (0, 0, V.fromList [0..255]) :: (Int, Int, V.Vector Int)
  let (_, _, result) = runRounds lengths 64 initialValues
  -- print result
  print $ toHexString $ condense V.empty result
