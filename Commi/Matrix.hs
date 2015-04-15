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
    let height = length mtx 
    let width = if height == 0 then 0 else length (mtx !! 0)
    let oldVal = if i < height && j < width then mtx !! i !! j else 0

    v <- inputInt (Just oldVal) `fire` OnChange <! [atr "size" "5"] 

    let newRow = listSet (if i < height then mtx !! i else []) j 0 v
    return $ listSet mtx i (replicate n 0) newRow

  fieldRow :: Int -> Widget [[Int]]
  fieldRow i = merge $ field i <$> [0 .. n-1] 

  rawMatrix = (merge $ (\i -> div ! atr "class" "row" <<< fieldRow i) <$> [0 .. n-1])
    -- <** (div ! atr "class" "row" <<< submitButton "Обновить")

merge :: [Widget a] -> Widget a 
merge [] = noWidget
merge (w:ws) = foldl (<|>) w ws