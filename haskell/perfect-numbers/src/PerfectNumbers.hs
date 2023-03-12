module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | aliquot n == n = Just Perfect
  | aliquot n > n = Just Abundant
  | aliquot n < n = Just Deficient
  | otherwise = Nothing
  where
    aliquot n = sum $ primeFactors n
    primeFactors n = filter (divides n) [1 .. n - 1]
    divides x y = x `mod` y == 0
