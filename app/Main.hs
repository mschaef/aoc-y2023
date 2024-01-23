module Main (main) where

import Control.Monad (forM_)
import Data.Either (lefts)

import Day1
import Day2

type DayResult = Either String String
data DayTest = DayTest String (IO Int) Int

resultDescription :: DayTest -> Int -> DayResult
resultDescription (DayTest name _ e) a =
  let description = name <> " - actual: " <> show a <> ", expected: " <> show e in
    if a == e
    then Right description
    else Left description

runTest :: DayTest -> IO DayResult
runTest dt = do
  let
    DayTest _ fn _ = dt
  actual <- fn
  return $ resultDescription dt actual

dayTests :: [ DayTest ]
dayTests = [DayTest "Day 1A" day1A 55208,
            DayTest "Day 1B" day1B 54578,
            DayTest "Day 2A" day2A 2406,
            DayTest "Day 2B" day2B 78375]

main :: IO ()
main = do
  results <- mapM runTest dayTests
  forM_ results (\r -> putStrLn $ show r)

  putStrLn $ "end run, failures: " <> show (length (lefts results))

