module Base (Error (..), rebase) where

import GHC.Float (int2Float)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inBase outBase inDigits
  | inBase < 2 = Left InvalidInputBase
  | outBase < 2 = Left InvalidOutputBase
  | not $ null invalidDigits = Left (InvalidDigit $ head invalidDigits)
  | null $ dropWhile (== 0) inDigits = Right []
  | otherwise = Right $ dropWhile (== 0) $ reverse $ map getOutDigit outPowers
  where
    invalidDigits = filter (\d -> d < 0 || d >= inBase) inDigits
    pow base power = product $ replicate power base
    input = sum $ zipWith (\d p -> d * pow inBase p) (reverse inDigits) [0 ..]
    outPowersOffBy1 = takeWhile (\d -> pow outBase d <= input) [0 ..]
    outPowers = outPowersOffBy1 ++ [last outPowersOffBy1 + 1]
    getOutDigit 0 = mod input outBase
    getOutDigit index = div (mod input $ pow outBase (index + 1)) (pow outBase index)