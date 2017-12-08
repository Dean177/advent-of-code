module Day_3 where
  
import Control.Monad.State
import Data.Maybe
import qualified Data.Map.Strict as Map
import Util

data Direction = N | E | S | W deriving (Eq, Show)

directionStream = [N, W, S, E] ++ directionStream

data StepCount = StepCount { _north::Int, _east::Int } deriving (Show)

hammingDistance :: StepCount -> Int
hammingDistance StepCount{_north=north, _east=east} = abs north + abs east

-- E 1, N 2, W 2, S 3, E 3

step :: Direction -> Int -> StepCount -> StepCount
step N x stepCount = stepCount { _north = _north stepCount + x }
step E x stepCount = stepCount { _east = _east stepCount + x }
step S x stepCount = stepCount { _north = _north stepCount - x }
step W x stepCount = stepCount { _east = _east stepCount - x }

data IterState = IterState {
    _stepsRemaining :: Int,
    _pathLength :: Int,
    _nextPathLength :: Int,
    _direction :: Direction,
    _directions :: [Direction],
    _steps :: StepCount
    } deriving (Show)
  
initialState target = IterState {
  _stepsRemaining = target - 1, -- `-1` as we are finised when we reach '1' rather than 0
  _pathLength = 1,
  _nextPathLength = 1,
  _direction = E,
  _directions = directionStream,
  _steps = StepCount { _north = 0, _east = 0 }
  }
  
walk :: State IterState Bool
walk = do 
  state@IterState{_pathLength=pathLength, _stepsRemaining=stepsRemaining, _nextPathLength=nextPathLength} <- get  
  let walkDistance = if stepsRemaining < pathLength then stepsRemaining else pathLength
  let hasArrived = stepsRemaining <= pathLength
  let nextDirection:nextDirections = _directions state
  put IterState{
    _stepsRemaining = stepsRemaining - walkDistance,
    _pathLength = _nextPathLength state,
    _nextPathLength = if pathLength == nextPathLength then nextPathLength + 1 else nextPathLength,
    _steps = step (_direction state) walkDistance (_steps state),
    _direction = nextDirection,
    _directions = nextDirections
    }

  return hasArrived
  
solve1 = untilS walk

-- Part 2
type Coord = (Int, Int) 
type PointValues = Map.Map Coord Int

evaluatedPoints :: PointValues
evaluatedPoints = Map.empty

lookupOrZero :: Coord ->  PointValues -> Int
lookupOrZero point points = fromMaybe 0 $ Map.lookup point points   

valueForCoord :: Coord -> PointValues -> Int
valueForCoord (x, y) points = 
  (lookupOrZero (x - 1, y - 1) points) + 
  (lookupOrZero (x - 1, y) points) +
  (lookupOrZero (x - 1, y + 1) points) + 
  (lookupOrZero (x, y + 1) points) +
  (lookupOrZero (x + 1, y + 1) points) +
  (lookupOrZero (x + 1, y) points) +
  (lookupOrZero (x + 1, y - 1) points) +
  (lookupOrZero (x, y - 1) points)
  
step1 :: Direction -> Coord -> Coord
step1 N (x, y) = (x, y + 1)
step1 E (x, y) = (x + 1, y)
step1 S (x, y) = (x, y - 1)
step1 W (x, y) = (x - 1, y)
  
solve2 :: Coord -> [Direction] -> Int -> Int -> Int -> PointValues -> Int
solve2 position directions distance distanceRemaining nextDistance pointValues =
  if nextValue > 265149 
  then nextValue
  else 
    solve2 
      nextPosition 
      nextDirections 
      distance'
      distanceRemaining'
      nextDistance'
      (Map.insert nextPosition nextValue pointValues)
  where 
    shouldChangeDirection = distanceRemaining == 1
    nextValue = valueForCoord nextPosition pointValues
    nextPosition = step1 (head directions) position
    nextDirections = 
      if shouldChangeDirection then (tail directions) else directions
    distance' = if shouldChangeDirection then nextDistance else distance
    distanceRemaining' =
      if shouldChangeDirection 
      then nextDistance 
      else distanceRemaining - 1
    nextDistance' = 
      if shouldChangeDirection && distance == nextDistance 
      then nextDistance + 1 
      else nextDistance
    
