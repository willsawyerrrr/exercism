module Dominoes (chain) where

import Data.List (delete)
import Data.Tuple (swap)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes
  | any odd $ counts dominoes = Nothing
  | null dominoes = Just []
  | null results = Nothing
  | otherwise = Just $ head results
  where
    results = filter valid $ hChain dominoes [[]]
    counts ds = hCounts ds $ replicate 10 0

hCounts :: [(Int, Int)] -> [Int] -> [Int]
hCounts [] cs = cs
hCounts ((x, y) : zs) cs = hCounts zs cs''
  where
    cs' = increment cs $ x - 1
    cs'' = increment cs' $ y - 1
    increment [] _ = []
    increment (a : as) 0 = (a + 1) : as
    increment (a : as) n = a : increment as (n - 1)

hChain :: [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]]
hChain [] chs = chs
hChain dominoes chs =
  [ ch'
    | domino <- dominoes,
      ch <- hChain (delete domino dominoes) chs,
      ch' <- [ch ++ [domino], ch ++ [swap domino]],
      interValid ch'
  ]

valid :: [(Int, Int)] -> Bool
valid ch = interValid ch && endsValid ch

interValid :: [(Int, Int)] -> Bool
interValid ch
  | length ch < 2 = True
  | otherwise = y == x' && interValid rest
  where
    rest = tail ch
    (_, y) = head ch
    (x', _) = head rest

endsValid :: [(Int, Int)] -> Bool
endsValid [] = True
endsValid [(x, y)] = x == y
endsValid ch = start == end
  where
    start = fst $ head ch
    end = snd $ last ch
