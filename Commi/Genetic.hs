{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Commi.Genetic where

import Data.List
import Control.Applicative
import Control.Monad 
import Control.Monad.Random 
import GHC.Generics (Generic)
import Control.DeepSeq
import System.IO.Unsafe 
import Haste.Foreign
import Haste.Prim
import Haste.JSON
import Haste.Serialize
import Haste

import Genetic.Coroutine 
import Genetic.Individ
import Genetic.State 
import Genetic.Population 

import Commi.Task
import Commi.Util

newtype CitiesIndivid = CitiesIndivid [Int]
  deriving (Show, Eq, Generic)

instance NFData CitiesIndivid

instance Individ CitiesIndivid where
  type IndividOptions CitiesIndivid = (Int, Int)

  crossover _ ia@(CitiesIndivid a) ib@(CitiesIndivid b)
    | length a /= length b = fail "Chromosomes have different lengths"
    | length a <= 1 = return (ia, ib)
    | length a == 2 = 
      let [a0, a1] = a 
          [b0, b1] = b
      in return (CitiesIndivid [a0, b1], CitiesIndivid [b0, a1])
    | otherwise = crossingover ia ib

  mutation _ ind@(CitiesIndivid []) = return ind
  mutation (_, cityn) (CitiesIndivid chr) = do                              --заменяем н-ый элемент инверсией
    n <- uniform [0..length chr - 1]
    --let arr = if n/=0 then take (n-1) chr ++ drop (n+1) chr else tail chr 
    
    let ni = chr `listIndex`  n 
    let arr2 = 
          if (ni /= 0 && ni /= -1) 
          then [-1 .. ni-2] ++ [ni+1 .. cityn]
          else [1 .. cityn]
    
    newA <- uniform arr2
    return $ CitiesIndivid $ take n chr ++ [newA] ++ drop (n+1) chr

  initIndivid (n, cityn) = CitiesIndivid . ([0] ++) <$> replicateM (n-1) randCity              --Добавить контакт [0]
        where randCity = uniform [-1..cityn-1]

        
crossingover::CitiesIndivid -> CitiesIndivid -> PauseableRand (CitiesIndivid, CitiesIndivid)
crossingover (CitiesIndivid a) (CitiesIndivid b) = do
        n <- uniform [0..length a -1]
        return $ f n
        where f n = (CitiesIndivid $ take n a ++ drop n b, CitiesIndivid $ take n b ++ drop n a)

extractSolution :: Input -> GeneticState CitiesIndivid -> Maybe Output
extractSolution input state = case findBest (fitness input) $ geneticPopulations state of
    Just (fit, CitiesIndivid ind) -> Just $ Output {
        outputSolution = ind 
      , outputCost = cost input $ CitiesIndivid ind 
      , outputFitness = fit
      }
    Nothing -> Nothing

cost :: Input -> CitiesIndivid -> Int
cost input (CitiesIndivid best) = sum $ map (\(a,b) -> ((inputCityMatrix input) `listIndex` a) `listIndex` b) $ zip (filter (/=(-1)) best) $ tail (filter (/=(-1)) best)

fitness :: Input -> CitiesIndivid -> Double
fitness input (CitiesIndivid chr) = if length chrPure == 0 
                    then -9001.0 
                    else bonus + cost + bonusnub
  where
    intCityMap = inputCityMatrix input
    chrPure = filter (/=(-1)) chr

    bonus::Double
    bonus = 100.0 * (fromIntegral $ length chr) / (fromIntegral $ length chrPure)

    cost :: Double
    cost = fromIntegral (sum $ map (\(a,b) -> (intCityMap `listIndex` a) `listIndex` b) $ zip chrPure $ tail chrPure)

    bonusnub ::Double
    bonusnub = fromIntegral $ (-300)*(length chr - (length $ nub chr) ) 

{-
-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: CitiesIndivid -> CitiesIndivid -> PauseableRand (CitiesIndivid, CitiesIndivid)
crossover3 (CitiesIndivid a) (CitiesIndivid b) = do
  [p1, p2, p3] <- sort <$> replicateM 3 (getRandomR (1, n - 2))
  let a' = concat [slice 0 p1 a, slice p1 p2 b, slice p2 p3 a, slice p3 n b]
  let b' = concat [slice 0 p1 b, slice p1 p2 a, slice p2 p3 b, slice p3 n a]
  return $ (CitiesIndivid a', CitiesIndivid b')
  where
    n = length a
    slice i1 i2 = take (i2 - i1) . drop i1

extractSolution :: Input -> GeneticState CitiesIndivid -> Maybe Output
extractSolution input state = case findBest (fitness input) $ geneticPopulations state of
    Just (fit, CitiesIndivid ind) -> Just $ Output (binToDouble (fromIntegral $ inputDigitsPrevDot input) ind) fit 
    Nothing -> Nothing

-- | Calculates fitness using user function
fitness :: Input -> CitiesIndivid -> Double
fitness input = fitGenerator (fromIntegral $ inputDigitsPrevDot input) (userFunction input) (inputExpected input)
  where
    fitGenerator :: Double -> (Double -> Double) -> Double -> CitiesIndivid -> Double
    fitGenerator num f target (CitiesIndivid chromosome) = 
      let x = binToDouble num chromosome
          delta = target - f x :: Double
      in (1.0 / delta ** 2.0)

userFunction :: Input -> Double -> Double 
userFunction input x = unsafePerformIO $ ffi (toJSStr $ "(" ++ inputFitness input ++ ")") x
-}