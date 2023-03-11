module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs = [parseToList $ substr i n xs | i <- [0 .. (length xs - n)]]
  where
    substr start length string = take length $ drop start xs
    parseToList = map digitToInt
