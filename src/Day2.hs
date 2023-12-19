module Day2
    ( day2A, day2B
    ) where

import Lib
import Text.Parsec

import Data.Either (rights)

import Control.Monad (void)

data MarbleCount = MarbleCount Int Int Int deriving (Show)

data MarbleGame = MarbleGame Int [ MarbleCount ] deriving (Show)

canBeDrawn :: MarbleCount -> MarbleCount -> Bool
canBeDrawn (MarbleCount dr dg db) (MarbleCount br bg bb) =
   dr <= br && dg <= bg && db <= bb

addCount :: MarbleCount -> MarbleCount -> MarbleCount
addCount (MarbleCount xr xg xb) (MarbleCount yr yg yb) =
   MarbleCount (xr + yr) (xg + yg) (xb + yb)

maxCount :: MarbleCount -> MarbleCount -> MarbleCount
maxCount (MarbleCount xr xg xb) (MarbleCount yr yg yb) =
   MarbleCount (max xr yr) (max xg yg) (max xb yb)

sumCounts :: [ MarbleCount ] -> MarbleCount
sumCounts draw = foldl addCount (MarbleCount 0 0 0) draw

minMarblesNeeded :: [ MarbleCount ] -> MarbleCount
minMarblesNeeded draw = foldl maxCount (MarbleCount 0 0 0) draw

numberParser:: Parsec String st Int
numberParser = read <$> (many1 $ oneOf "0123456789")

whitespace :: Parsec String st ()
whitespace = void $ many $ oneOf " \n\t"

lineHeaderParser :: Parsec String st Int
lineHeaderParser = (string "Game") *> whitespace *> numberParser <* (char ':')

marbleParser :: Parsec String st MarbleCount
marbleParser = do
  whitespace
  c <- numberParser
  whitespace
  color <- (string "red" <|> string "green" <|> string "blue")
  return (case color of
    "red" -> MarbleCount c 0 0
    "green"-> MarbleCount 0 c 0
    "blue" -> MarbleCount 0 0 c
    _ -> MarbleCount 0 0 0)

marbleDrawParser :: Parsec String st MarbleCount
marbleDrawParser = do
  draws <- sepBy marbleParser (char ',')
  return $ sumCounts draws

lineParser :: Parsec String st MarbleGame
lineParser = do
  gameId <- lineHeaderParser
  draws <- sepBy marbleDrawParser (char ';')
  return (MarbleGame gameId draws)

gameCanBePlayed :: MarbleGame -> Bool
gameCanBePlayed (MarbleGame _ draws) =
  null (filter (not . (\t -> canBeDrawn t (MarbleCount 12 13 14))) draws)

parseGameLine :: String -> Either ParseError MarbleGame
parseGameLine line = parse lineParser "" line

sumGameIds :: [ MarbleGame ] -> Int
sumGameIds games = sum (map (\ (MarbleGame gameId _) ->  gameId) games)

loadGames :: IO [ MarbleGame ]
loadGames = do
  fl <- fileLines "input-day-2.txt"

  return $ rights (map parseGameLine fl)

gamePower :: MarbleGame -> Int
gamePower (MarbleGame _ draws) = do
  let (MarbleCount r g b) = minMarblesNeeded draws
  r * g * b

day2A :: IO ()
day2A = do
  games <- loadGames

  let total = sumGameIds (filter gameCanBePlayed games)

  putStrLn $ "Day 2A sum = " <> show total

day2B :: IO ()
day2B = do

  games <- loadGames

  let total = sum (map gamePower games)

  putStrLn $ "Day 2B sum = " <> show total

