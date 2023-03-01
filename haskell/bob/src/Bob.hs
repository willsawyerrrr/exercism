module Bob (responseFor) where

import Data.Char (isAsciiLower, isAsciiUpper, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
    | null $ dropWhileEnd isSpace xs                                                            = "Fine. Be that way!"
    | last (dropWhileEnd isSpace xs) == '?' && not (any isAsciiLower xs) && any isAsciiUpper xs = "Calm down, I know what I'm doing!"
    | last (dropWhileEnd isSpace xs) == '?'                                                     = "Sure."
    | not (any isAsciiLower xs) && any isAsciiUpper xs                                          = "Whoa, chill out!"
    | otherwise                                                                                 = "Whatever."
