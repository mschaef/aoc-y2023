module Main (main) where

import Day1
import Day2

resultDescription :: Int -> Int -> String
resultDescription a e =
  if a == e
     then "PASS - received expected value: " <> show a
  else "FAIL - actual " <> show a <> " different from expected " <> show e


runTest :: String -> IO Int -> Int -> IO ()
runTest name fn expected = do
  result <- fn
  putStrLn $ name <> " " <> resultDescription result expected

main :: IO ()
main = do

  runTest "Day 1A" day1A 55208
  runTest "Day 1B" day1B 54578

  runTest "Day 2A" day2A 2406
  runTest "Day 2B" day2B 78375

  putStrLn "end run."

