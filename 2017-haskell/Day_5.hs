module Day_5 where
  
import qualified Data.Sequence as Seq

parseInt = read :: String -> Int
parse = Seq.fromList . map parseInt . lines

type Instructions = Seq.Seq Int

data ComputationState = CompState { 
  instructionIndex :: Int, 
  count :: Int,
  instructions :: Seq.Seq Int
  } deriving (Show)
  
instruction :: ComputationState -> Int
instruction compState = Seq.index 
  (instructions compState) 
  (instructionIndex compState)

isOutOfBounds :: Int -> Int -> Bool
isOutOfBounds bound i = i < 0 || i >= bound
 
nextIndex :: ComputationState -> Int
nextIndex compState = 
  instructionIndex compState + instruction compState

continueComputation :: ComputationState -> ComputationState
continueComputation state = CompState { 
  instructionIndex = nextIndex state, 
  count = (count state) + 1,
  instructions = Seq.update 
    (instructionIndex state) 
    (instruction state + 1) 
    (instructions state)
  }

solve bound = 
  until 
    (\s -> isOutOfBounds bound (instructionIndex s))
    continueComputation  

-- Part 2
incBy :: Int -> Int
incBy instruc = if instruc >= 3 then -1 else 1

continueComputation_2 :: Int -> Int -> Int -> Instructions -> Int
continueComputation_2 bound count index instructions = 
  if (isOutOfBounds bound index) 
  then count  
  else continueComputation_2 bound (count + 1) nextIndex updatedInstructions
  where
    currenInstruction = (Seq.index instructions index) :: Int
    nextIndex = index + currenInstruction 
    updatedInstructions = 
      Seq.update 
        index
        (currenInstruction + (incBy currenInstruction))
        instructions  




