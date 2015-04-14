{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Commi.Task where

import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Genetic.Options

data Input = Input {
    inputCityMatrix :: [[Int]]
  , inputCityN :: Int 
  , inputIndividLength :: Int 
  , inputGeneticOptions :: GeneticOptions
} deriving (Typeable, Show)

instance Serialize Input where
  toJSON i = Dict [
      ("inputCityMatrix", toJSON $ inputCityMatrix i)
    , ("inputCityN", toJSON $ inputCityN i)
    , ("inputIndividLength", toJSON $ inputIndividLength i)
    , ("inputGeneticOptions", toJSON $ inputGeneticOptions i)
    ] 
  parseJSON j = Input 
    <$> j .: "inputCityMatrix"
    <*> j .: "inputCityN"
    <*> j .: "inputIndividLength"
    <*> j .: "inputGeneticOptions"

initialInput :: Input
initialInput = Input {
    inputCityMatrix = 
      [[-1000,  -5,   -10,  -30,  -25,    -40,    -15,    -10,  -25,    -5,     -15,  -10],
      [-5,    -1000,  -20,  -40,    -18,    -20,    -30,    -5,     -15,    -10,  -25,    -15],
      [-10,   -20,  -1000,  -15,  -40,    -15,    -5,   -15,    -5,     -40,    -20,    -40],
      [-30,   -40,  -15,  -1000,  -15,  -35,    -25,    -50,  -10,  -25,    -5,     -30],
      [-25,   -18,  -40,    -15,  -1000,  -25,    -10,  -20,  -15,  -50,    -10,  -25],
      [-40,   -20,    -15,    -35,    -25,    -1000,  -5,     -30,    -30,    -70,    -5,     -35],
      [-15,   -30,    -5,   -25,    -10,  -5,   -1000,  -10,  -20,  -15,    -30,    -5],
      [-10,   -5,     -15,    -50,    -20,    -30,    -10,  -1000,  -25,    -30,    -40,    -5],
      [-25,   -15,    -5,     -10,  -15,    -30,  -20,  -25,    -1000,  -15,    -10,  -18],
      [-5,    -10,  -40,    -25,    -50,    -70,    -15,    -30,    -15,    -1000,  -20,  -20],
      [-15,   -25,    -20,    -5,     -10,  -5,     -30,    -40,    -10,  -20,    -1000,  -5],
      [-10,   -15,  -40,    -30,    -25,    -35,    -5,     -5,     -18,  -20,  -5,     -1000]]
  , inputGeneticOptions = initialOptions
  , inputCityN = 2
  , inputIndividLength = 5
  }

data Output = Output {
  outputSolution :: [Int],
  outputCost :: Int,
  outputFitness :: Double
} deriving (Typeable, Show)

data PlotState = PlotState{
  values :: [(Double, Double)] -- ^ Points: x - generation number, y - fitness value
} deriving (Typeable, Show)

initialPlotState :: PlotState 
initialPlotState = PlotState []