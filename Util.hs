module Util where

import Control.Monad.ST
import Control.Monad.State
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V

untilS :: State a Bool -> a -> a
untilS statefulComp initState = if result then nextState else untilS statefulComp nextState
  where (result, nextState) = runState statefulComp initState

rotateR :: V.Unbox a => Int -> V.MVector s a -> ST s (V.MVector s a)
rotateR 0 vec = return vec
rotateR distance vec = do
  copy <- MV.new (MV.length vec) :: V.Unbox a => ST s (V.MVector s a)
  MV.copy copy vec

  MV.copy
    (MV.slice distance (MV.length vec - distance) vec)
    (MV.slice 0 (MV.length vec - distance) copy)

  MV.copy
    (MV.slice 0 distance vec)
    (MV.slice (MV.length vec - distance) distance copy)

  return vec
