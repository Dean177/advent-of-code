module Day_6 where
  
import Control.Monad.State
import Control.Monad.Loops (iterateWhile)
import Data.Foldable
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set


parseInt = read :: String -> Int
parse :: String -> Seq.Seq Int
parse input = Seq.fromList $ parseInt <$> words input

zipWithIndex :: Seq.Seq a -> Seq.Seq (Int, a)
zipWithIndex xs = (Seq.fromFunction (Seq.length xs) id) `Seq.zip` xs
 
comp a@(_, a') b@(_, b') = 
  if a' > b' 
  then a
  else (if a' < b' then b else a)
  
maxWithIndex :: Seq.Seq (Int, Int) -> (Int, Int)
maxWithIndex = foldl comp (0, -1)

-- Get values to wrap around
wrapIndex seqLength targetIndex = targetIndex `mod` seqLength

--data PState = PState { 
--  _count :: Int,
--  _memory :: Seq.Seq (Int, Int),
--  _previousStates :: Set.Set (Seq.Seq (Int, Int))
--  } deriving (Show)

incrementIndexes :: Int -> Int -> Seq.Seq (a, Int) -> Seq.Seq (a, Int)
incrementIndexes startFrom 0 seqs = seqs
incrementIndexes startFrom remainingTotal seqs = 
  incrementIndexes 
    (startFrom + 1) 
    (remainingTotal - 1) 
    (Seq.update targetIndex (i, v + 1) seqs) 
    where 
      targetIndex = wrapIndex (Seq.length seqs) startFrom
      (i, v) = Seq.index seqs targetIndex  

--iter :: State PState Bool
--iter = do
--  PState {_count=count, _memory=memory, _previousStates=previousStates} <- get
--  let (index, value) = maxWithIndex memory
--  let withZeroedCell = Seq.update index (index, 0) memory
--  let redistributedMemory = incrementIndexes (index + 1) value withZeroedCell
--  let hasSeenMemoryStateBefore = Set.member redistributedMemory previousStates
--  put PState { 
--    _count = count + 1, 
--    _memory = redistributedMemory, 
--    _previousStates=Set.insert redistributedMemory previousStates 
--    }
--    
--  return hasSeenMemoryStateBefore
  
solve s = if (fst s) then s else solve $ runState iter (snd s)
 
-- Part 2 
data PState = PState { 
  _count :: Int,
  _memory :: Seq.Seq (Int, Int),
  _previousStates :: Set.Set (Seq.Seq (Int, Int)),
  _previousSeq :: Seq.Seq (Seq.Seq (Int, Int))
  } deriving (Show)
iter :: State PState Bool
iter = do
  PState {_count=count, _memory=memory, _previousStates=previousStates, _previousSeq=previousSeq} <- get
  let (index, value) = maxWithIndex memory
  let withZeroedCell = Seq.update index (index, 0) memory
  let redistributedMemory = incrementIndexes (index + 1) value withZeroedCell
  let hasSeenMemoryStateBefore = Set.member redistributedMemory previousStates
  put PState { 
    _count = count + 1, 
    _memory = redistributedMemory, 
    _previousStates = Set.insert redistributedMemory previousStates,
    _previousSeq = previousSeq Seq.|> redistributedMemory
    }
    
  return hasSeenMemoryStateBefore
  

 