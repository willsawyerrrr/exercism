module Garden
  ( Plant (..),
    garden,
    lookupPlants,
  )
where

import Data.List (elemIndex)

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

data Garden = Garden
  { students :: [String],
    firstRow :: [Plant],
    secondRow :: [Plant]
  }

garden :: [String] -> String -> Garden
garden students plants = Garden students firstRow secondRow
  where
    firstRow = map toPlant $ head (lines plants)
    secondRow = map toPlant $ last (lines plants)
    toPlant plant
      | plant == 'C' = Clover
      | plant == 'G' = Grass
      | plant == 'R' = Radishes
      | plant == 'V' = Violets

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = [firstRow garden !! (2 * studentIndex), firstRow garden !! (2 * studentIndex + 1), secondRow garden !! (2 * studentIndex), secondRow garden !! (2 * studentIndex + 1)]
  where
    Just studentIndex = elemIndex student $ students garden
