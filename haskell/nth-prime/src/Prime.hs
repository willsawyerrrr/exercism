module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ filter isPrime [2 ..] !! (n - 1)
  where
    isPrime x = primeFactors x == [1, x]
    primeFactors x = filter (divides x) [1 .. x]
    divides x y = x `mod` y == 0
