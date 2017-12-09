module Day_6 where

import Control.Monad.State
import Control.Monad.Loops (iterateWhile)
import Data.Foldable
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

parseInt = read :: String -> Int
parse :: String -> Seq.Seq Int
parse input = Seq.fromList $ parseInt <$> words input

zipWithIndex :: Seq.Seq a -> Seq.Seq (Int, a)
zipWithIndex xs = Seq.fromFunction (Seq.length xs) id `Seq.zip` xs

comp a@(_, a') b@(_, b')
  | a' > b' = a
  | a' < b' = b
  | otherwise = a

type Memory = Seq.Seq (Int, Int)

maxWithIndex :: Memory -> (Int, Int)
maxWithIndex = foldl comp (0, -1)

-- Get values to wrap around
wrapIndex seqLength targetIndex = targetIndex `mod` seqLength

data PState = PState {
 _count :: Int,
 _memory :: Memory,
 _previousStates :: Set.Set Memory
 } deriving (Show)

incrementIndexes :: Int -> Int -> Memory -> Memory
incrementIndexes startFrom 0 seqs = seqs
incrementIndexes startFrom remainingTotal seqs =
  incrementIndexes
    (startFrom + 1)
    (remainingTotal - 1)
    (Seq.update targetIndex (i, v + 1) seqs)
    where
      targetIndex = wrapIndex (Seq.length seqs) startFrom
      (i, v) = Seq.index seqs targetIndex

iter :: State PState Bool
iter = do
  PState {_count=count, _memory=memory, _previousStates=previousStates} <- get
  let (index, value) = maxWithIndex memory
  let withZeroedCell = Seq.update index (index, 0) memory
  let redistributedMemory = incrementIndexes (index + 1) value withZeroedCell
  let hasSeenMemoryStateBefore = Set.member redistributedMemory previousStates
  put PState {
    _count = count + 1,
    _memory = redistributedMemory,
    _previousStates=Set.insert redistributedMemory previousStates
    }

  return hasSeenMemoryStateBefore

solve s = if fst s then s else solve $ runState iter (snd s)

-- Part 2

solve2 :: Memory -> Set.Set Memory -> Seq.Seq Memory -> Seq.Seq Memory
solve2 memory previousStates pastIterations =
  if hasSeenMemoryStateBefore
  then pastIterations Seq.|> redistributedMemory
  else solve2
    redistributedMemory
    (Set.insert redistributedMemory previousStates)
    (pastIterations Seq.|> redistributedMemory)
  where
    hasSeenMemoryStateBefore = Set.member redistributedMemory previousStates
    (index, value) = maxWithIndex memory
    withZeroedCell = Seq.update index (index, 0) memory
    redistributedMemory = incrementIndexes (index + 1) value withZeroedCell

main :: IO Int
main = do
  -- let puzzleInput = zipWithIndex $ parse "0 2 7 0"
  let puzzleInput = zipWithIndex $ parse "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
  let memorySequences = solve2 puzzleInput Set.empty Seq.empty
  let lastSequence = Seq.index memorySequences (Seq.length memorySequences - 1)
  let firstIndex = Seq.findIndexL (== lastSequence) memorySequences

  return $ Seq.length memorySequences - 1 - fromMaybe 0 firstIndex
