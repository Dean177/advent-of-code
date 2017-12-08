module Util where
  
import Control.Monad.State

untilS :: (State a Bool) -> a -> a
untilS statefulComp initState = if result then nextState else untilS statefulComp nextState
  where (result, nextState) = runState statefulComp initState
  