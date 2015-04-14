{-# LANGUAGE OverloadedStrings #-}
module Commi.Application where

import Prelude hiding (div)
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Applicative
import Commi.Task
import Commi.Util
import Commi.Genetic
import Commi.Plot
import Commi.Config
import System.Random
import Haste.HPlay.View hiding (head)
import Haste
import Haste.Graphics.Canvas
import Genetic.Options
import Genetic.Solve 
import Genetic.State
import Genetic.Coroutine

data ApplicationState = AppConfigure Input 
  | AppCalculate Input PlotState (GeneticState CitiesIndivid) (Maybe (Pauseable (GeneticState CitiesIndivid)))
  | AppShow Input PlotState Output

data Route = RouteConfig | RouteCalculate | RouteShow
  deriving (Enum, Show)

initialState :: ApplicationState
initialState = AppConfigure initialInput

runApplication :: ApplicationState -> Widget ()
runApplication state = wloop state go
  where 
    go :: ApplicationState -> Widget ApplicationState
    go localState@(AppConfigure input) = do
      update <- eitherWidget (fieldConfigWidget input) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteCalculate -> do
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState Nothing
          _ -> fail $ "invalid route in config state " ++ show route
        Left newInput -> return $ AppConfigure newInput

    go localState@(AppCalculate input plotState geneticState coroutine) = do
      update <- eitherWidget (geneticWidget input geneticState plotState coroutine) $ routeWidget localState
      case update of 
        Right route -> do
          liftIO $ clearTimers
          case route of 
            RouteConfig -> return $ AppConfigure input 
            RouteShow -> return $ AppShow input plotState $ fromJust $ extractSolution input geneticState
            _ -> fail $ "invalid route in config state " ++ show route
        Left (newGeneticState, newPlotState, newCoroutine) -> return $ if isGeneticFinished newGeneticState 
          then AppShow input newPlotState $ fromJust $ extractSolution input newGeneticState
          else AppCalculate input newPlotState newGeneticState newCoroutine

    go localState@(AppShow input plotState output) = do
      update <- eitherWidget (showResultsWidget input plotState output) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteConfig -> return $ AppConfigure input 
          RouteCalculate -> do 
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState Nothing
          _ -> fail $ "invalid route in show state " ++ show route
        Left _ -> return localState

eitherWidget :: Widget a -> Widget b -> Widget (Either a b)
eitherWidget wa wb = (return . Left =<< wa) <|> (return . Right =<< wb)

routeWidget :: ApplicationState -> Widget Route
routeWidget state = div ! atr "class" "row" 
  <<< div ! atr "class" "col-md-4 col-md-offset-5"
  <<< go state
  where
    go (AppConfigure {}) = bigBtn RouteCalculate "Начать эволюцию"
    go (AppCalculate {}) = bigBtn RouteConfig "Назад"  <|> bigBtn RouteShow "Остановить"
    go (AppShow {}) = bigBtn RouteConfig "Начать с начала" <|> bigBtn RouteCalculate "Перерасчитать"

    bigBtn v s = cbutton v s <! [atr "class" "btn btn-primary btn-lg"]

geneticWidget :: Input -> GeneticState CitiesIndivid -> PlotState -> Maybe (Pauseable (GeneticState CitiesIndivid)) -> Widget (GeneticState CitiesIndivid, PlotState, Maybe (Pauseable (GeneticState CitiesIndivid)))
geneticWidget input geneticState plotState coroutine = do 
  --wprint $ show $ geneticCurrentBest geneticState

  let newPlotState =  if null $ geneticPopulations geneticState
                      then plotState
                      else plotState 
                      { 
                        values = values plotState ++ [
                          ( fromIntegral $ geneticCurrentGen geneticState, 
                            fromMaybe 0 $ fst <$> geneticCurrentBest geneticState
                          )] 
                      }

  (dwidth, dheight) <- liftIO $ getDocumentSize
  let maybeSolution = outputSolution <$> extractSolution input geneticState
  div ! atr "class" "col-md-12" <<< do
    plotWidget newPlotState [] "Поколение" "Фитнес" ( 0.8 * fromIntegral dwidth, fromIntegral dheight * 0.5) 0 2 []
    wraw $ panel "Текущий результат" $ mconcat [
        labelRow 4 "Лучший фитнес: " $ show $ maybe 0 fst $ geneticCurrentBest geneticState
      , labelRow 4 "Текущий ответ: " $ maybe "" show maybeSolution
      , labelRow 4 "Стоимость пути: " $ maybe "" (\ind -> show $ cost input ind) $ CitiesIndivid <$> maybeSolution
      ]
  --resultPlot input dwidth dheight maybeSolution

  corRes <- timeout 200 $ liftIO $ case coroutine of 
    Nothing -> resume $ solve 
      (inputIndividLength input, inputCityN input) 
      (fitness input) (inputGeneticOptions input) geneticState
    Just cr -> resume cr
  (newGeneticState, newCoroutine) <- case corRes of 
    Left (Yield _ paused) -> return (geneticState, Just paused)
    Right genst -> return (genst, Nothing)
  
  return (newGeneticState, newPlotState, newCoroutine)

showResultsWidget :: Input -> PlotState -> Output -> Widget ()
showResultsWidget input plotState output = do
  (dwidth, dheight) <- liftIO $ getDocumentSize
  let maybeSolution = Just $ outputSolution output

  div ! atr "class" "row" <<< do
    -- div ! atr "class" "col-md-6" <<< resultPlot input dwidth dheight maybeSolution
    div ! atr "class" "col-md-12" <<< plotWidget plotState [] "Поколение" "Фитнес" 
      (fromIntegral dwidth * 0.8, fromIntegral dheight * 0.8) 0 2 []
  div ! atr "class" "row" <<< do
    wraw $ div ! atr "class" "row-fluid" $ mconcat [
        div ! atr "class" "col-md-6" $ inputInfo
      , div ! atr "class" "col-md-6" $ optionsInfo
      , div ! atr "class" "col-md-6" $ outputInfo
      , div ! atr "class" "col-md-6" $ otherInfo
      ]
    noWidget
  where
    opts = inputGeneticOptions input 

    inputInfo = panel "Входные данные" $ mconcat [
        labelRow 6 "Количество городов:" $ show $ inputCityN input
      , labelRow 6 "Длина индивидов:" $ show $ inputIndividLength input
      ]

    optionsInfo = panel "Настройки эволюции" $ mconcat [
        labelRow 6 "Шанс мутации: " $ show $ mutationChance opts
      , labelRow 6 "Часть элиты: " $ show $ elitePart opts
      , labelRow 6 "Максимальное число поколений: " $ show $ maxGeneration opts
      , labelRow 6 "Кол-во популяций: " $ show $ popCount opts
      , labelRow 6 "Кол-во индивидов в популяции: " $ show $ indCount opts
      , labelRow 6 "Ожидаемое значение фитнеса: " $ maybe "" show $ targetFitness opts
      ]

    outputInfo = panel "Результаты эволюции" $ mconcat [
        labelRow 6 "Лучший фитнес: " $ show $ outputFitness output
      , labelRow 6 "Лучшее решение: " $ show $ outputSolution output
      ]

    otherInfo = panel "Другая информация" $ mconcat [
        labelRow 6 "Стоимость пути: " $ show $ cost input (CitiesIndivid $ outputSolution output)
      ]