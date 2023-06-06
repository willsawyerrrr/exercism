module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a, b, c) | a <- [1 .. div sum 3], b <- [a .. sum - 2 * a], let c = sum - a - b, a ^ 2 + b ^ 2 == c ^ 2]
