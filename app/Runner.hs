module Runner (
  runTests,
  DayTest (DayTest)) where

import Control.Monad (forM_)
import Data.Either (lefts)

type DayResult = Either String String
data DayTest = DayTest String ([ String ] -> Int) String Int

fileLines :: String -> IO [ String ]
fileLines fileName = lines <$> readFile fileName

resultDescription :: DayTest -> Int -> DayResult
resultDescription (DayTest name _ _ e) a =
  let description = name <> " - actual: " <> show a <> ", expected: " <> show e in
    if a == e
    then Right description
    else Left description

runTest :: DayTest -> IO DayResult
runTest dt = do
  let
    DayTest _ fn filename _ = dt
  inputLines <- fileLines filename
  return $ resultDescription dt (fn inputLines)

runTests :: [ DayTest ] -> IO ()
runTests tests = do
  results <- mapM runTest tests
  forM_ results (\r -> case r of
                         Right desc -> putStrLn $ "Pass: " <> desc
                         Left desc -> putStrLn $ "FAIL: " <> desc)

  putStrLn $ "end run, failures: " <> show (length (lefts results))
