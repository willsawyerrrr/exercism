module Yacht (yacht, Category (..)) where

import Data.List

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht
  deriving (Eq)

yacht :: Category -> [Int] -> Int
yacht category dice
  | category == Ones = 1 * count 1
  | category == Twos = 2 * count 2
  | category == Threes = 3 * count 3
  | category == Fours = 4 * count 4
  | category == Fives = 5 * count 5
  | category == Sixes = 6 * count 6
  | category == FullHouse = if different == 2 && (length (head grouped) `elem` [2, 3]) then sum dice else 0
  | category == FourOfAKind && different == 1 = 4 * head dice
  | category == FourOfAKind && different == 2 = if length (head grouped) `elem` [1, 4] then 4 * dice !! 3 else 0
  | category == LittleStraight = if different == 5 && last sorted == 5 then 30 else 0
  | category == BigStraight = if different == 5 && head sorted == 2 then 30 else 0
  | category == Choice = sum dice
  | category == Yacht = if different == 1 then 50 else 0
  where
    count value = length $ filter (== value) dice
    sorted = sort dice
    grouped = group sorted
    different = length grouped
