module Main (main) where

import Day1
import Day2

runTest :: String -> IO Int -> IO ()
runTest name fn = do
  result <- fn
  putStrLn $ name <> " result = " <> show result

main :: IO ()
main = do

  runTest "Day 1A" day1A
  runTest "Day 1B" day1B

  runTest "Day 2A" day2A
  runTest "Day 2B" day2B

  putStrLn "end run."

