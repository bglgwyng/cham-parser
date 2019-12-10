module Main where

import Data.Aeson
import Text.Megaparsec
import Prelude hiding (putStrLn)
import Data.String.Class (putStrLn)

import Lib

main :: IO ()
main = do
  input <- getContents
  putStrLn $ show options
  case parse source "" input of
    Left error -> putStrLn $ errorBundlePretty error
    Right ast -> putStrLn $ encode ast
  return ()