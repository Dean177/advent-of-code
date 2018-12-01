module Day_1 where

makePairs input = zip input $ drop 1 input ++ take 1 input

matchingPairs = filter $ uncurry (==)

parseInt char = read (char:"") :: Int
parseFst = parseInt . fst

solve input = sum $ map parseFst $ matchingPairs $ makePairs input

-- Part 2
makePairs_2 input = zip input $ drop min_point input ++ take min_point input
  where min_point = div (length input) 2

solve_2 input = sum $ fmap parseFst $ matchingPairs $ makePairs_2 input

main :: IO ()
main = do
  input <- readFile "./Day_1.txt"
  print $ solve input
  print $ solve_2 input
