{-# LANGUAGE RecordWildCards #-}
module Day_10 where

import Control.Monad.ST (runST, ST)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import Text.Printf (printf)

hashSize :: Int
hashSize = 256

wrapToHashSize :: Int -> Int
wrapToHashSize i = i `mod` hashSize

salt :: [Int]
salt = [17, 31, 73, 47, 23]

data KnotHashRing s = KnotHashRing {
  position :: !Int,
  skip :: !Int,
  hash :: !(MV.STVector s Int)
  }

initKnotHash :: ST s (KnotHashRing s)
initKnotHash = fmap (KnotHashRing 0 0) (V.thaw (V.fromList [0 .. hashSize - 1]))

twist :: KnotHashRing s -> Int -> ST s (KnotHashRing s)
twist KnotHashRing {..} n = do
  let indexes = wrapToHashSize <$> [position, position + 1 .. position + n - 1]
  xs <- traverse (MV.unsafeRead hash) indexes
  traverse_ (uncurry $ MV.unsafeWrite hash) (zip indexes (reverse xs))
  return $ KnotHashRing (wrapToHashSize $ position + skip + n) (skip + 1) hash

sparseHash :: [Int] -> [Int]
sparseHash lengths = V.toList $ runST $ do
  knotHashRing <- initKnotHash
  KnotHashRing {..} <- foldlM twist knotHashRing lengths
  V.unsafeFreeze hash

parseLengths :: String -> [Int]
parseLengths input = ord <$> input
-- parseLengths input = fromIntegral . ord <$> input

knotHashDay10 :: String -> [Int]
knotHashDay10 input =
  let lengths = parseLengths input ++ salt
      expand = concat . replicate 64
  in  foldl1' xor <$> chunksOf 16 (sparseHash $ expand lengths)

knotHash :: String -> [Word8]
knotHash input =
  let lengths = parseLengths input ++ salt
      expand = concat . replicate 64
      sparse = sparseHash $ expand lengths
      word8s = fromIntegral <$> sparse :: [Word8]
  in  foldl1' xor <$> chunksOf 16 word8s

part1 :: String -> Int
part1 = check . sparseHash . readInts
 where
  check (a:b:_) = a * b
  readInts = fmap read . splitOn ","

part2 :: String -> String
part2 = concatMap (printf "%02x") . knotHashDay10

main = do
  let puzzleInput = "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205"
  print $ part1 puzzleInput
  print $ part2 puzzleInput
