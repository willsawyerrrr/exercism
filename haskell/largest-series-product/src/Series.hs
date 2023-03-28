module Series (Error (..), largestProduct) where

import Data.Char (isDigit)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | not (null notDigit) = Left $ InvalidDigit (head notDigit)
  | otherwise = Right $ maximum products
  where
    notDigit = dropWhile isDigit digits
    products = map product numLists
    numLists = map parseInts $ substrings digits
    parseInts = map (\char -> read [char] :: Integer)
    substrings string = map (\n -> take size $ drop n string) [0 .. (length string - size)]
