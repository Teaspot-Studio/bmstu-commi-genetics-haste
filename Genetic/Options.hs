{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Genetic.Options where

import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative

data GeneticOptions = GeneticOptions {
  mutationChance :: Double,
  elitePart :: Double,
  maxGeneration :: Int,
  popCount :: Int,
  indCount :: Int,
  targetFitness :: Maybe Double
} deriving (Typeable, Show)

instance Serialize GeneticOptions where
  toJSON o = Dict $ [
      ("mutationChance", toJSON $ mutationChance o)
    , ("elitePart", toJSON $ elitePart o)
    , ("maxGeneration", toJSON $ maxGeneration o)
    , ("popCount", toJSON $ popCount o)
    , ("indCount", toJSON $ indCount o)
    ] ++ if isJust $ targetFitness o 
      then [("targetFitness", toJSON $ fromJust $ targetFitness o)]
      else []

  parseJSON j = GeneticOptions
    <$> j .: "mutationChance"
    <*> j .: "elitePart"
    <*> j .: "maxGeneration"
    <*> j .: "popCount"
    <*> j .: "indCount"
    <*> j .:? "targetFitness"

initialOptions :: GeneticOptions
initialOptions = GeneticOptions {
    popCount = 1,
    indCount = 50,
    maxGeneration = 500,
    targetFitness = Just 500,
    mutationChance = 0.8,
    elitePart = 0.1
  }