{-# LANGUAGE OverloadedStrings #-}
module Day_21 where

import Data.List (foldl1')
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)

type Grid = Matrix Bool
type Rule = (Grid, Grid)

cell :: Parser Bool
cell = (char '.' *> pure False) <|> (char '#' *> pure True)

matrix :: Parser (Matrix Bool)
matrix = Matrix.fromLists <$> (many cell `sepBy` char '/')

ruleParser :: Parser Rule
ruleParser = (,) <$> matrix <*> (string " => " *> matrix)

rulesParser :: Parser [Rule]
rulesParser = ruleParser `sepBy` char '\n'

instance Ord a => Ord (Matrix a) where
  ma `compare` mb = case size ma `compare` size mb of
    EQ -> compRows 1 ma mb
    sizeCompare -> sizeCompare
    where
      compRow i a b = Matrix.getRow i a `compare` Matrix.getRow i b
      compRows i a b
        | i == size ma = compRow i a b
        | otherwise = case compRow i a b of
            EQ -> compRows (i + 1) a b
            r -> r

size :: Matrix a -> Int
size = Matrix.nrows

chunk :: Grid -> Int -> Int -> Int-> Grid
chunk grid n x y = Matrix.submatrix row (row + n - 1) column (column + n - 1) grid
  where -- submatrix uses 1 based indexing!?
    column = x + 1
    row = y + 1

chunkRow :: Grid -> Int -> [Grid]

chunks :: Grid -> [[Grid]]
chunks grid = go <*> indexes
  where
    go :: [Int -> [Grid]]
    go = [chunk grid chunkSize] `fmap` indexes
    indexes = filter ((== 0) . (`mod` chunkSize)) [0..(size grid - 1)]
    chunkSize = if size grid `mod` 2 == 0 then 2 else 3

flipY :: Grid -> Grid
flipY = Matrix.fromLists . fmap reverse . Matrix.toLists -- This isnt in Data.Matrix 😞

rotateR :: Grid -> Grid
rotateR = flipY . Matrix.transpose

permutations :: Rule -> [Rule]
permutations rule@(input, output) = flip (,) output <$> inputs
  where
    inputs :: [Grid]
    inputs = transformations <*> [input]
    rotations = [id, rotateR, rotateR . rotateR, rotateR . rotateR . rotateR]
    transformations :: [Grid -> Grid]
    transformations = rotations ++ ((. flipY) <$> rotations)

findExpansions :: [Rule] -> Grid -> Grid
findExpansions rules = (Map.!) expansionMap
  where
    expansionMap :: Map Grid Grid
    expansionMap = Map.fromList $ concatMap permutations rules

expand :: [Rule] -> Grid -> Grid
expand rules = glue . (fmap . fmap) expansionRules . chunks
  where expansionRules = findExpansions rules

glue :: [[Grid]] -> Grid
glue = foldl1' (Matrix.<->) . fmap (foldl1' (Matrix.<|>))

toDotHash b = if b then 1 else 0

inputMatrix :: Grid
inputMatrix = Matrix.fromLists [[False, True, False], [False, False, True], [True, True, True]]



main :: IO ()
main = do
  -- (Right rules) <- parse rulesParser "Matrix" <$> TIO.readFile "./Day_21.txt"
  let (Right rules) = parse rulesParser "'Test' rules" "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
  print $ length $ concatMap permutations rules
  -- let fifthExpansion = Matrix.toList $ iterate (expand rules) inputMatrix !! 2
  -- putStr $ Matrix.prettyMatrix $ (toDotHash <$> fst input)
  -- print $ chunks inputMatrix

  -- print $ length $ filter id fifthExpansion

  -- print $ expandChunk (head $ head (chunks inputMatrix))
  -- print $ permutations $ head rules
  return ()
-- {-# Language ViewPatterns #-}
-- module Main where
--
-- import           Data.List
-- import           Data.List.Split
-- import qualified Data.Map as Map
--
-- count f x= length $ filter f x
--
-- main :: IO ()
-- main =
--   do input <- parseInput <$> readFile "./Day_21.txt"
--
--      let rules      = makeRules input
--          iterations = iterate (mapSubSquares rules) start
--
--      print (count ('#'==) (concat (iterations !!  5)))
--      print (count ('#'==) (concat (iterations !! 18)))
--
--
-- type Grid = [String]
--
--
-- -- | Initial grid value (a game of life glider).
-- start :: Grid
-- start = [".#.", "..#", "###"]
--
--
-- -- | Generate all of the rotated and flipped versions of a grid.
-- similarSquares :: Grid -> [Grid]
-- similarSquares x = take 4 . iterate rotateCCW =<< [x, reverse x]
--
--
-- -- | Rotate a grid counter-clockwise.
-- rotateCCW :: Grid -> Grid
-- rotateCCW = reverse . transpose
--
--
-- -- | Apply a function to all of the subsquares of a grid.
-- mapSubSquares :: (Grid -> Grid) -> Grid -> Grid
-- mapSubSquares rules xs =
--   map concat . transpose . map rules . transpose . map (chunksOf n)
--   =<< chunksOf n xs
--   where
--     n | even (length xs) = 2
--       | otherwise        = 3
--
--
-- -- | Build the grid update function given the list of rules
-- -- loaded from the input file.
-- makeRules :: [(Grid, Grid)] -> Grid -> Grid
-- makeRules rs =
--   let rulesMap = Map.fromList [ (k',v) | (k,v) <- rs , k' <- similarSquares k ]
--   in (rulesMap Map.!)
--
--
-- -- | Parse a string a list of grid rules.
-- parseInput :: String -> [(Grid,Grid)]
-- parseInput = map parseRule . lines
--
-- -- | Parse a string as a rule mapping one grid to another.
-- parseRule :: String -> (Grid,Grid)
-- parseRule (words -> [a,"=>",b]) = (splitOn "/" a, splitOn "/" b)
