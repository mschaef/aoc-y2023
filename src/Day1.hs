module Day1
    ( day1A, day1B
    ) where

import Data.List (find, isPrefixOf, tails)
import Data.Maybe (catMaybes)

type DigitStringList = [(String, Int)]

digitStringsA :: DigitStringList
digitStringsA = [
  ("0", 0),
  ("1", 1),
  ("2", 2),
  ("3", 3),
  ("4", 4),
  ("5", 5),
  ("6", 6),
  ("7", 7),
  ("8", 8),
  ("9", 9)]

digitStringsB :: DigitStringList
digitStringsB = [
  ("0", 0), ("zero" , 0),
  ("1", 1), ("one"  , 1),
  ("2", 2), ("two"  , 2),
  ("3", 3), ("three", 3),
  ("4", 4), ("four" , 4),
  ("5", 5), ("five" , 5),
  ("6", 6), ("six"  , 6),
  ("7", 7), ("seven", 7),
  ("8", 8), ("eight", 8),
  ("9", 9), ("nine" , 9)]

digitValAt :: DigitStringList -> String -> Maybe Int
digitValAt digitStrings atStr = fmap snd $ find (\( s, _ ) -> isPrefixOf s atStr) digitStrings

digits :: DigitStringList -> String ->  [ Int ]
digits digitStrings s = catMaybes $ map (digitValAt digitStrings) (tails s)

lineValue :: DigitStringList -> String -> Int
lineValue digitStrings line = do
  let lineDigits = digits digitStrings line
  (head lineDigits) * 10 + (last lineDigits)

day1 :: DigitStringList -> [ String ] -> Int
day1 digitStrings inputLines = sum (map (lineValue digitStrings) inputLines)

day1A :: [ String ] -> Int
day1A inputLines = day1 digitStringsA inputLines

day1B :: [ String ] -> Int
day1B inputLines = day1 digitStringsB inputLines
