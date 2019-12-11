module Main where

import           Data.Aeson
import           Data.String.Class (putStrLn)
import           Prelude           hiding (putStrLn)
import           System.Exit
import           Text.Megaparsec

import           Lib

main :: IO ()
main = do
  input <- getContents
  case parse source "" input of
    Left error -> die $ errorBundlePretty error
    Right ast  -> putStrLn $ encode ast
  return ()
