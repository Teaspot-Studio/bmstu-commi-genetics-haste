module Commi.Matrix where

import Prelude hiding (id, div)
import Haste.HPlay.View hiding (head)
import Commi.Task

matrixWidget :: Input -> Int -> Widget [[Int]]
matrixWidget n = mapM [0 .. n-1] (\j -> div ! atr "class" "row" <<< mapM [0 .. n-1] $ \i -> field i j)
  <** (div ! atr "class" "row" <<< submitButton "Обновить")
  where
  n = inputCityN input 
  mtx = inputCityMatrix input
  field i j = inputInt (Just $ mtx !! i !! j) 