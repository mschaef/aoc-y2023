module Day1
    ( day1
    ) where

import Data.List (find, isPrefixOf, tails)
import Data.Maybe (catMaybes)

import Lib

digitStrings :: [(String, Int)]
digitStrings = [
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

digitValAt :: String -> Maybe Int
digitValAt atStr = fmap snd $ find (\( s, _ ) -> isPrefixOf s atStr) digitStrings

digits :: String ->  [ Int ]
digits s = catMaybes $ map digitValAt (tails s)

lineValue :: String -> Int
lineValue line = do
  let lineDigits = digits line
  (head lineDigits) * 10 + (last lineDigits)

day1 :: IO ()
day1 = do
  fl <- fileLines "input-day-1.txt"
  let total = sum (map lineValue fl)

  putStrLn $ "Day 1 sum = " <> show total
