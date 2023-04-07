module BinarySearch (find) where

import Data.Array
import Data.Foldable (toList)

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = hFind arr x 0 $ length arr

hFind :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
hFind arr val lo hi
  | null list = Nothing
  | midElem == val = Just mid
  | lo == hi = Nothing
  | midElem > val = hFind arr val lo mid
  | midElem < val = hFind arr val (mid + 1) hi
  where
    list = toList arr
    mid = min (length list - 1) (div (hi + lo) 2)
    midElem = list !! mid
