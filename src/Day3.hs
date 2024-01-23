module Day3
    ( day3A
    ) where

import Data.Char (isDigit)
import Data.List (tails)

isFlagChar :: Char -> Bool
isFlagChar ch = ch /= '.' && not (isDigit ch)

lineFlagMask :: String -> [ Bool ]
lineFlagMask l = map isFlagChar l

windows' :: Int -> [a] -> [[a]]
windows' n = map (take n) . tails

lineFlagMasks :: [ String ] -> [ [ Bool ] ]
lineFlagMasks inputLines =
  let masks = map lineFlagMask inputLines
      bookend = map (\_ -> False) (head masks)
  in [ bookend ] ++ masks ++ [ bookend ]

day3A :: [ String ] ->  Int
day3A inputLines =
  let indexed = take (length inputLines) $ windows' 3 (lineFlagMasks inputLines) in
    length indexed
