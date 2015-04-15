module Commi.Matrix where

import Prelude hiding (id, div)
import Haste.HPlay.View hiding (head)
import Control.Monad
import Commi.Task
import Commi.Util

matrixWidget :: Input -> Widget Input
matrixWidget input = do
  raw <- rawMatrix
  return $ input {
      inputCityMatrix = raw
    }
  where
  n = inputCityN input 
  mtx = inputCityMatrix input

  field :: Int -> Int -> Widget [[Int]]
  field i j = do
    v <- inputInt (Just $ mtx !! i !! j) <! [atr "size" "5"]
    return $ listSet mtx i (listSet (mtx !! i) j v)

  fieldRow :: Int -> Widget [[Int]]
  fieldRow i = merge $ field i <$> [0 .. n-1] 

  rawMatrix = (merge $ (\i -> div ! atr "class" "row" <<< fieldRow i) <$> [0 .. n-1])
    <** (div ! atr "class" "row" <<< submitButton "Обновить")

merge :: [Widget a] -> Widget a 
merge [] = noWidget
merge (w:ws) = foldl (<|>) w ws