module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)
import Numeric (showInt)

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ length digits) digits)
  where
    digits = getDigits n
    getDigits 0 = []
    getDigits n = mod n 10 : getDigits (quot n 10)
