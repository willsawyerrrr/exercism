module Proverb (recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = []
recite list = intercalate "\n" (helper $ list ++ [head list])
  where
    helper :: [String] -> [String]
    helper [_, final] = [concat ["And all for the want of a ", final, "."]]
    helper (first : rest) = concat ["For want of a ", first, " the ", head rest, " was lost."] : helper rest
