module Lib
    ( day1
    ) where

import Data.Char (isDigit, ord)
import Data.List (tails)

import Data.Maybe (catMaybes)

digitValAt :: String -> Maybe Int
digitValAt (ch :_) = if isDigit ch then Just ((ord ch) - (ord '0')) else Nothing
digitValAt _ = Nothing

digits :: String ->  [ Int ]
digits s = catMaybes $ map digitValAt (tails s)

lineValue :: String -> Int
lineValue line = do
  let lineDigits = digits line
  (head lineDigits) * 10 + (last lineDigits)

fileLines :: String -> IO [ String ]
fileLines fileName = lines <$> readFile fileName

day1 :: IO ()
day1 = do
  fl <- fileLines "input-day-1.txt"
  let total = sum (map lineValue fl)

  putStrLn $ "Sum = " <> show total

  putStrLn "end run."
