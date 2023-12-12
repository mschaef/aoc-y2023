module Lib
    ( day1
    ) where

import Data.Char (isDigit, ord)
import Data.List (tails)

digitValAt :: String -> Int
digitValAt (ch :_) = if isDigit ch then (ord ch) - (ord '0') else -1
digitValAt _ = -1

digits :: String ->  [ Int ]
digits s = filter (\c -> c >= 0) $ map digitValAt (tails s)

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
