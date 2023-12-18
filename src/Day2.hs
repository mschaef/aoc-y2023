module Day2
    ( day2
    ) where

import Lib
import Text.Parsec

import Data.Either (rights)

import Control.Monad (void)

data MarbleCount = MarbleCount Int Int Int deriving (Show)

data MarbleGame = MarbleGame Int [ [ MarbleCount ] ] deriving (Show)

canBeDrawn :: MarbleCount -> MarbleCount -> Bool
canBeDrawn (MarbleCount dr dg db) (MarbleCount br bg bb) =
   dr <= br && dg <= bg && db <= bb

addCount :: MarbleCount -> MarbleCount -> MarbleCount
addCount (MarbleCount xr xg xb) (MarbleCount yr yg yb) =
   MarbleCount (xr + yr) (xg + yg) (xb + yb)

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
    "blue" -> MarbleCount 0 0 c)

marbleDrawParser :: Parsec String st [ MarbleCount ]
marbleDrawParser = sepBy marbleParser (char ',')

lineParser :: Parsec String st MarbleGame
lineParser = do
  gameId <- lineHeaderParser
  draws <- sepBy marbleDrawParser (char ';')
  return (MarbleGame gameId draws)

drawTotalMarbles :: [ MarbleCount ] -> MarbleCount
drawTotalMarbles draw = foldl addCount (MarbleCount 0 0 0) draw

gameCanBePlayed :: MarbleGame -> Bool
gameCanBePlayed (MarbleGame _ draws) =
  null (filter (not . (\t -> canBeDrawn t (MarbleCount 12 13 14))) (map drawTotalMarbles draws))

lineValue :: String -> Either ParseError MarbleGame
lineValue line = parse lineParser "" line

sumIds games = sum (map (\ (MarbleGame id _) ->  id) games)

day2 :: IO ()
day2 = do
  fl <- fileLines "input-day-2.txt"

  let total = sumIds (filter gameCanBePlayed (rights (map lineValue fl)))

  putStrLn $ "Day 2 sum = " <> show total
