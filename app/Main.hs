module Main (main) where

import Runner (
  runTests,
  DayTest (DayTest)
  )

import Day1
import Day2
import Day3

main :: IO ()
main = do
  runTests [DayTest "Day 1A" day1A "input-day-1.txt" 55208,
            DayTest "Day 1B" day1B "input-day-1.txt" 54578,
            DayTest "Day 2A" day2A "input-day-2.txt" 2406,
            DayTest "Day 2B" day2B "input-day-2.txt" 78375,
            DayTest "Day 3A" day3A "input-day-3.txt" (-1)]
 
