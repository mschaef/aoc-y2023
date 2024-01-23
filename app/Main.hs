module Main (main) where

import Runner (
  runTests,
  DayTest (DayTest)
  )

import Day1
import Day2

main :: IO ()
main = do
  runTests [DayTest "Day 1A" day1A 55208,
            DayTest "Day 1B" day1B 54578,
            DayTest "Day 2A" day2A 2406,
            DayTest "Day 2B" day2B 78375]
