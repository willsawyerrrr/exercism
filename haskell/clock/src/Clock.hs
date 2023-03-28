module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock
  { hours :: Int,
    minutes :: Int
  }
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = addDelta hour min $ Clock 0 0

toString :: Clock -> String
toString clock = concat [hoursString, ":", minutesString]
  where
    hoursString = (if hours clock < 10 then "0" else "") ++ show (hours clock)
    minutesString = (if minutes clock < 10 then "0" else "") ++ show (minutes clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = Clock actualHours actualMins
  where
    rawMins = minutes clock + min
    actualMins = rawMins `mod` 60
    rawHours = hours clock + hour + (rawMins `div` 60)
    actualHours = rawHours `mod` 24
