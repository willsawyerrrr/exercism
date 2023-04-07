module Brackets (arePaired) where

openers :: [Char]
openers = ['[', '{', '(']

closers :: [Char]
closers = [']', '}', ')']

arePaired :: String -> Bool
arePaired str = hArePaired str []

hArePaired :: String -> [Char] -> Bool
hArePaired [] [] = True
hArePaired [] _ = False
hArePaired str open
  | head str `elem` openers = hArePaired (tail str) (head str : open)
  | head str `elem` closers = closerMatches (head str) && hArePaired (tail str) (tail open)
  | otherwise = hArePaired (tail str) open
  where
    closerMatches ']' = not (null open) && head open == '['
    closerMatches '}' = not (null open) && head open == '{'
    closerMatches ')' = not (null open) && head open == '('
    closerMatches _ = False
