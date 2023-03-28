module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs
  | not $ null invalid = Left $ head invalid
  | otherwise = Right $ map transcribe xs
  where
    invalid = dropWhile (`elem` "GCTA") xs
    transcribe x
      | x == 'G' = 'C'
      | x == 'C' = 'G'
      | x == 'T' = 'A'
      | x == 'A' = 'U'
