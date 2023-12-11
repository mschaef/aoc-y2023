module Lib
    ( day1
    ) where

import Data.Char (isDigit, ord)

digits :: String -> String
digits s = filter isDigit s

digitVal :: Char -> Int
digitVal ch = (ord ch) - (ord '0')

lineValue :: String -> Int
lineValue line = do
  let lineDigits = digits line
  digitVal (head lineDigits) * 10 + digitVal (last lineDigits)

fileLines :: String -> IO [ String ]
fileLines fileName = lines <$> readFile fileName

day1 :: IO ()
day1 = do
  fl <- fileLines "input-day-1.txt"
  let total = sum (map lineValue fl)

  putStrLn $ "Sum = " <> show total

  putStrLn "end run."
