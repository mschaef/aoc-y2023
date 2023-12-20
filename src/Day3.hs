module Day3
    ( day3A
    ) where

import Lib
import Data.Maybe (catMaybes)
import Data.Char (isDigit)
import Data.List (tails)

isFlagChar :: Char -> Bool
isFlagChar ch = ch /= '.' && not (isDigit ch)

lineFlagMask :: String -> [ Bool ]
lineFlagMask l = map isFlagChar l

windows' :: Int -> [a] -> [[a]]
windows' n = map (take n) . tails

lineFlagMasks :: [ String ] -> [ [ Bool ] ]
lineFlagMasks lines =
  let masks = map lineFlagMask lines
      bookend = map (\_ -> False) (head masks)
  in [ bookend ] ++ masks ++ [ bookend ]

day3A :: IO Int
day3A = do
  fl <- fileLines "input-day-3-small.txt"

  let indexed = take (length fl) $ windows' 3 (lineFlagMasks fl)

  mapM_ print indexed

  putStrLn $ "Day 3A" <> show indexed
  return 0
