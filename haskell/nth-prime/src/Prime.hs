module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ primes !! (n - 1)
  where
    primes = [i | i <- [1 ..], isPrime i]
    isPrime x = divisors x == [1, x]
    divisors x = [i | i <- [1 .. x], divides x i]
    divides x y = x `mod` y == 0

-- need to implement a sieve to check primality more efficiently
