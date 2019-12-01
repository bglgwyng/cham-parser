module Main where

import Text.Megaparsec

import Lib

import Document

main :: IO ()
main = do
  -- parseTest document "#[ssdfd]"
  input <- getContents
  parseTest root input
  -- parseTest emptyComment input
  return ()