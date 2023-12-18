module Lib
    ( fileLines
    ) where

fileLines :: String -> IO [ String ]
fileLines fileName = lines <$> readFile fileName